open Syntax.Type_system
open Syntax.Expression
open Core

type context = {
  mutable variables : (string, data_type) Hashtbl.t;
  items : (string, item) Hashtbl.t;
}

let clone_context ctx =
  { variables = Hashtbl.copy ctx.variables; items = Hashtbl.copy ctx.items }

exception Mismatched_types
exception Unknown_definition of string
exception Unsupported of string

let rec compute_expression_type expr ctx =
  match expr with
  | LiteralExpression lit -> (
      match lit with Number (f, _) -> NumericType (f, None) | Str _ -> String)
  | VariableExpression var -> Hashtbl.find_exn ctx.variables var
  | BinaryExpression bin_expr ->
      let lhs_type = compute_expression_type bin_expr.lhs ctx in
      let rhs_type = compute_expression_type bin_expr.rhs ctx in
      if type_equals lhs_type rhs_type then lhs_type else raise Mismatched_types
  | CallExpression (name, _) -> (
      match Hashtbl.find_exn ctx.items name with
      | FunctionDefinition func_def -> func_def.return_type
      | _ -> raise (Unknown_definition name))
  (*| AccessExpression access_expr -> String*)
  | StructConstructionExpression (name, _) -> NamedType name

let compute_items items =
  let result = Hashtbl.create (module String) in
  let add_item_to_result (i : item) : unit =
    Hashtbl.add_exn result ~key:(name_of_item i) ~data:i
  in
  List.iter items ~f:add_item_to_result;
  result

let rec type_exists ty ctx =
  match ty with
  | VoidType -> true
  | String -> true
  | NumericType _ -> true
  | NamedType name -> (
      match Hashtbl.find ctx.items name with
      | Some (StructDefinition _) -> true
      | _ -> false)
  | Tuple obj -> List.for_all obj ~f:(fun x -> type_exists x ctx)

let verify_struct struct_def ctx =
  let names = Hashtbl.create (module String) in
  let add_field (name, ty) =
    Hashtbl.add_exn names ~key:name ~data:();
    if not (type_exists ty ctx) then
      raise
        (Unknown_definition
           ("Unknown type of field " ^ name ^ " in struct "
          ^ struct_def.struct_name))
  in
  List.iter struct_def.fields ~f:add_field;
  struct_def

let on_def_expr_lists (expr_list : (string * expression) list)
    (data_type_list : (string * data_type) list)
    (f : string -> expression -> data_type -> 'a) : 'a list =
  let named_expr_table = Hashtbl.create (module String) in
  List.iter
    ~f:(fun (name, expr) ->
      Hashtbl.add_exn named_expr_table ~key:name ~data:expr)
    expr_list;
  if Hashtbl.length named_expr_table <> List.length data_type_list then
    raise (Unknown_definition "Expression count mismatch")
  else if
    not
      (List.for_all
         ~f:(fun (name, _) ->
           Option.is_some (Hashtbl.find named_expr_table name))
         data_type_list)
  then raise (Unknown_definition "Expression list name mismatch")
  else
    let join_expr_and_d_type (name, d_type) =
      let expr = Hashtbl.find_exn named_expr_table name in
      f name expr d_type
    in
    List.map ~f:join_expr_and_d_type data_type_list
    (* this should sort it in the correct order, but it may be a point of error *)

let rec update_literal_types expr num_ty ctx =
  let update_expr_with_dtype name expr d_type =
    let sub_literal_type =
      match d_type with NumericType (f, _) -> Some f | _ -> num_ty
    in
    (name, update_literal_types expr sub_literal_type ctx)
  in

  match expr with
  | LiteralExpression lit -> (
      match lit with
      | Number (old_num_ty, f) ->
          let new_ty = Option.value num_ty ~default:old_num_ty in
          LiteralExpression (Number (new_ty, f))
      | Str str -> LiteralExpression (Str str))
  | VariableExpression name -> VariableExpression name
  | BinaryExpression bin_expr ->
      BinaryExpression
        {
          op = bin_expr.op;
          lhs = update_literal_types bin_expr.lhs num_ty ctx;
          rhs = update_literal_types bin_expr.rhs num_ty ctx;
        }
  | CallExpression (name, arg_exprs) -> (
      match Hashtbl.find_exn ctx.items name with
      | FunctionDefinition func_def ->
          let named_exprs =
            match
              List.map2
                ~f:(fun expr (name, _) -> (name, expr))
                arg_exprs func_def.parameters
            with
            | Ok a -> a
            | _ ->
                raise
                  (Unknown_definition "Function argument and parameter unequal")
          in
          let processed_exprs =
            on_def_expr_lists named_exprs func_def.parameters
              update_expr_with_dtype
          in
          CallExpression (name, List.map ~f:(fun (_, a) -> a) processed_exprs)
      | _ -> raise (Unknown_definition "Unknown struct"))
  | StructConstructionExpression (name, field_exprs) -> (
      match Hashtbl.find_exn ctx.items name with
      | StructDefinition struct_def ->
          StructConstructionExpression
            ( name,
              on_def_expr_lists field_exprs struct_def.fields
                update_expr_with_dtype )
      | _ -> raise (Unknown_definition "Unknown struct"))

let verify_function func_def ctx =
  let add_parameter (name, ty) =
    Hashtbl.add_exn ctx.variables ~key:name ~data:ty;
    if not (type_exists ty ctx) then
      raise
        (Unknown_definition
           ("Unknown type of parameter " ^ name ^ " in function "
          ^ func_def.name))
  in
  List.iter func_def.parameters ~f:add_parameter;
  let verify_statement stmt =
    match stmt with
    | Expression expr ->
        let retyped_expr = update_literal_types expr None ctx in
        if not (type_exists (compute_expression_type retyped_expr ctx) ctx) then
          raise
            (Unknown_definition ("Unknown expression type in " ^ func_def.name))
        else Expression retyped_expr
    | Assignment (var, d_type, expr) ->
        let num_type =
          match d_type with NumericType (f, _) -> Some f | _ -> None
        in
        let retyped_expr = update_literal_types expr num_type ctx in
        let expr_type = compute_expression_type retyped_expr ctx in
        if not (type_exists expr_type ctx) then
          raise
            (Unknown_definition ("Unknown statement type in " ^ func_def.name))
        else if not (type_equals expr_type d_type) then
          raise (Unknown_definition "Assigment expression type mismatch")
        else Hashtbl.set ctx.variables ~key:var ~data:d_type;
        Assignment (var, d_type, retyped_expr)
  in
  let statements, final_expr = func_def.block in
  let retyped_statements = List.map ~f:verify_statement statements in
  let ret_num_type =
    match func_def.return_type with NumericType (f, _) -> Some f | _ -> None
  in
  let retyped_final_expr = update_literal_types final_expr ret_num_type ctx in
  let final_expr_type = compute_expression_type retyped_final_expr ctx in
  if not (type_exists final_expr_type ctx) then
    raise (Unknown_definition ("Unknown statement type in " ^ func_def.name))
  else if not (type_equals final_expr_type func_def.return_type) then
    raise (Unknown_definition "Return expression type mismatch")
  else
    {
      name = func_def.name;
      parameters = func_def.parameters;
      return_type = func_def.return_type;
      block = (retyped_statements, retyped_final_expr);
    }

let verify_item item ctx =
  match item with
  | StructDefinition struct_def ->
      StructDefinition (verify_struct struct_def ctx)
  | EnumDefinition _ -> raise (Unsupported "Enum types aren't supported yet")
  | FunctionDefinition func_def ->
      FunctionDefinition (verify_function func_def (clone_context ctx))
  | UnionDefinition _ -> raise (Unsupported "Union types aren't supported yet")

let verify_typing (items : item list) =
  let ctx =
    { variables = Hashtbl.create (module String); items = compute_items items }
  in
  List.map items ~f:(fun x -> verify_item x ctx)
