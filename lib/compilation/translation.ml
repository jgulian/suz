open Syntax.Type_system
open Syntax.Expression
open Llvm
open Core

exception Unsupported_feature of string
exception Failed_to_desugar of string

let simplify_module (items : Syntax.Expression.item list) =
  let simplify_item i =
    match i with
    | StructDefinition def -> StructDefinition def
    | FunctionDefinition def -> FunctionDefinition def
    | EnumDefinition def -> EnumDefinition def
    | UnionDefinition def -> UnionDefinition def
  in
  List.map ~f:simplify_item items

type codegen_context = {
  mutable context : llcontext;
  mutable current_module : llmodule;
}

let test_size a b = 
  Option.value ~default:false (Option.map ~f:(fun x -> x = b) a)

let map_type (ty : data_type) (ctx : codegen_context) =
  match ty with
  | VoidType -> void_type ctx.context
  | String -> raise (Unsupported_feature "string")
  | NumericType (format, size) -> (
      match format with
      | Float ->
          if test_size size 4 then float_type ctx.context
          else if test_size size 8 || Option.is_none size then double_type ctx.context
          else
            raise (Unsupported_feature "floating types of size other than 4/8")
      | Signed | Unsigned ->
          integer_type ctx.context (Option.value ~default:8 size))
  | NamedType name -> Option.value_exn (type_by_name ctx.current_module name)
  | Tuple _ -> raise (Unsupported_feature "tuples are not supported")

let codegen_struct (def : struct_definition) ctx =
  let ty = Option.value_exn (type_by_name ctx.current_module def.struct_name) in
  let map_field (_, ty) = map_type ty ctx in
  let field_types = Array.of_list (List.map ~f:map_field def.fields) in
  struct_set_body ty field_types false


type function_context = { 
  mutable named_values: (string, llvalue) Hashtbl.t; 
  mutable count : int;
  mutable builder : llbuilder;
  }

let rec expression_codegen expr ctx func_ctx = 
  match expr with
  | LiteralExpression lit ->
    begin 
      match lit with
        | Number (format, value) -> 
          begin 
            match format with
              | Signed | Unsigned -> 
                const_int (i64_type ctx.context) (int_of_float value) 
              | Float -> const_float (double_type ctx.context) value
          end
        | Str _ -> raise (Unsupported_feature "strings")  
    end
  | VariableExpression name -> Hashtbl.find_exn func_ctx.named_values name
  | BinaryExpression { op; lhs; rhs; } -> 
    let lhs_value = expression_codegen lhs ctx func_ctx in
    let rhs_value = expression_codegen rhs ctx func_ctx in
    let ty = classify_type (type_of rhs_value) in 
    let id = string_of_int func_ctx.count in
    func_ctx.count <- func_ctx.count + 1;
    begin 
      match op with 
      | Add -> 
        begin 
          match ty with 
          | Float | Double -> build_fadd lhs_value rhs_value id func_ctx.builder
          | Integer -> build_add lhs_value rhs_value id func_ctx.builder
          | _ -> raise (Unsupported_feature "operator usage on types")  
        end
      | Sub -> begin 
          match ty with 
          | Float | Double -> build_fsub lhs_value rhs_value id func_ctx.builder
          | Integer -> build_sub lhs_value rhs_value id func_ctx.builder
          | _ -> raise (Unsupported_feature "operator usage on types")  
        end
      | Mul -> begin 
          match ty with 
          | Float | Double -> build_fmul lhs_value rhs_value id func_ctx.builder
          | Integer -> build_mul lhs_value rhs_value id func_ctx.builder
          | _ -> raise (Unsupported_feature "operator usage on types")  
        end
      | Div -> begin 
          match ty with 
          | Float | Double -> build_fdiv lhs_value rhs_value id func_ctx.builder
          | Integer -> build_sdiv lhs_value rhs_value id func_ctx.builder
          | _ -> raise (Unsupported_feature "operator usage on types")  
        end
      | Less | Greater -> raise (Unsupported_feature "comparisons") 
      | Access -> raise (Unsupported_feature "access") 
    end
  | CallExpression (name, arg_exprs) -> 
    let map_args = fun e -> expression_codegen e ctx func_ctx in 
    let args = Array.of_list (List.map ~f:map_args arg_exprs) in 
    let func = Option.value_exn (lookup_function name ctx.current_module) in
    let id = string_of_int func_ctx.count in
    func_ctx.count <- func_ctx.count + 1;
    build_call func args id func_ctx.builder
  | StructConstructionExpression (name, _) -> 
    (* Need to store fields later *)
    let ty = named_struct_type ctx.context name in
    let id = string_of_int func_ctx.count in
    func_ctx.count <- func_ctx.count + 1;
    build_alloca ty id func_ctx.builder

let function_body_codegen (def: function_definition) ctx = 
  let func = Option.value_exn (lookup_function def.name ctx.current_module) in
  let entry = entry_block func in 
  let builder = builder_at_end ctx.context entry in
  let named_values = Hashtbl.create (module String) in 
  let function_ctx = { named_values; count = 1; builder; } in
  let statement_codegen stmt = 
    match stmt with
      | Expression expr -> let _ = expression_codegen expr ctx function_ctx in ()
      | Assignment (name, _, expr) -> 
        let value = expression_codegen expr ctx function_ctx in 
        Hashtbl.set function_ctx.named_values ~key:name ~data:value
      in
  let (statements, final_expression) = def.block in
  List.iter ~f:statement_codegen statements;
  expression_codegen final_expression ctx function_ctx
 

let codegen_module (items : item list) (ctx : codegen_context) =
  let codegen_structures_signatures i =
    match i with
    | StructDefinition def ->
        let _ = named_struct_type ctx.context def.struct_name in
        ()
    | UnionDefinition def ->
        let _ = named_struct_type ctx.context def.union_name in
        ()
    | _ -> ()
  in
  let codegen_function_signatures i =
    match i with 
    | FunctionDefinition def -> 
      let return_type = map_type def.return_type ctx in
      let parameters = List.map ~f:(fun (_, x) -> map_type x ctx) def.parameters in
      let parameter_array = Array.of_list parameters in 
      let function_ty = function_type return_type parameter_array in
      let _ = define_function def.name function_ty ctx.current_module in ()
    | _ -> ()
  in
  let codegen_function_bodies i =
    match i with
    | FunctionDefinition def -> 
      let return_type = map_type def.return_type ctx in
      let parameters = List.map ~f:(fun (_, x) -> map_type x ctx) def.parameters in
      let parameter_array = Array.of_list parameters in 
      let function_ty = function_type return_type parameter_array in
      let _ = define_function def.name function_ty ctx.current_module in ()
    | _ -> ()
  in
  List.iter ~f:codegen_structures_signatures items;
  List.iter ~f:codegen_function_signatures items;
  List.iter ~f:codegen_function_bodies items;
  dump_module ctx.current_module

let create_context _ =
  let context = global_context () in
  let current_module = create_module context "main" in
  { context; current_module }
