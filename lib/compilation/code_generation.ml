open Structure
open Llvm
open Tsr
open Core

exception Unexpected_type of string
exception Internal_issue of string

let rec get_data_type dt context =
  match dt with
  | Void -> void_type context
  | Bool -> i1_type context
  | Numeric format -> (
      match format with
      | Floating -> double_type context
      | Signed -> i64_type context
      | Unsigned -> i64_type context)
  | Tuple factors ->
      struct_type context
        (Array.of_list (List.map ~f:(fun x -> get_data_type x context) factors))
  | Named (name, _) -> named_struct_type context name

let generate_operator_functions context current_mod =
  let generate_operator_function op ty inst =
    let name = Lowering.map_op_to_name op ty in
    let return_ty = get_data_type (Lowering.op_return_type op ty) context in
    let ty = get_data_type ty context in
    let func_ty = function_type return_ty (Array.of_list [ ty; ty ]) in
    let func = define_function name func_ty current_mod in
    let builder = builder_at_end context (entry_block func) in
    let a, b =
      match Array.to_list (params func) with
      | [ a; b ] -> (a, b)
      | _ ->
          raise
            (Internal_issue
               ("incorrect number of arguments in operator function " ^ name))
    in
    let _ = build_ret (inst a b "" builder) builder in
    ()
  in
  let li = { Ast.file = ""; lines = (0, 0); columns = (0, 0) } in
  generate_operator_function (Ast.Add li) (Numeric Unsigned) build_add;
  generate_operator_function (Ast.Add li) (Numeric Signed) build_add;
  generate_operator_function (Ast.Add li) (Numeric Floating) build_fadd;
  generate_operator_function (Ast.Sub li) (Numeric Unsigned) build_sub;
  generate_operator_function (Ast.Sub li) (Numeric Signed) build_sub;
  generate_operator_function (Ast.Sub li) (Numeric Floating) build_fsub;
  generate_operator_function (Ast.Mul li) (Numeric Unsigned) build_mul;
  generate_operator_function (Ast.Mul li) (Numeric Signed) build_mul;
  generate_operator_function (Ast.Mul li) (Numeric Floating) build_fmul;
  generate_operator_function (Ast.Equals li) (Numeric Unsigned)
    (build_icmp Icmp.Eq);
  generate_operator_function (Ast.Equals li) (Numeric Signed)
    (build_icmp Icmp.Eq);
  generate_operator_function (Ast.Equals li) (Numeric Floating)
    (build_fcmp Fcmp.Oeq);
  generate_operator_function (Ast.NotEquals li) (Numeric Unsigned)
    (build_icmp Icmp.Ne);
  generate_operator_function (Ast.NotEquals li) (Numeric Signed)
    (build_icmp Icmp.Ne);
  generate_operator_function (Ast.NotEquals li) (Numeric Floating)
    (build_fcmp Fcmp.One);
  generate_operator_function (Ast.Less li) (Numeric Unsigned)
    (build_icmp Icmp.Ult);
  generate_operator_function (Ast.Less li) (Numeric Signed)
    (build_icmp Icmp.Slt);
  generate_operator_function (Ast.Less li) (Numeric Floating)
    (build_fcmp Fcmp.Olt);
  generate_operator_function (Ast.Greater li) (Numeric Unsigned)
    (build_icmp Icmp.Ugt);
  generate_operator_function (Ast.Greater li) (Numeric Signed)
    (build_icmp Icmp.Sgt);
  generate_operator_function (Ast.Greater li) (Numeric Floating)
    (build_fcmp Fcmp.Ogt)

let rec codegen_expr expr build_context =
  let context, current_mod, builder, named_values = build_context in
  match expr with
  | Literal (v, dt, li) -> (
      match dt with
      | Numeric format -> (
          match format with
          | Floating -> const_float (get_data_type dt context) v
          | Signed | Unsigned ->
              const_int (get_data_type dt context) (int_of_float v))
      | _ ->
          raise
            (Unexpected_type
               (Printf.sprintf "unexpected type of literal at (%s)"
                  (Ast.print_li li))))
  | Variable (name, _, li) ->
      let error =
        Printf.sprintf "unknown variable reference %s at (%s)" name
          (Ast.print_li li)
      in
      let pointer = Hashtbl.find named_values name in
      let pointer, _ = Option.value_exn ~message:error pointer in
      build_load pointer "" builder
  | Call (function_name, args, _, li) ->
      let func_error =
        Printf.sprintf "unknown function reference %s at (%s)" function_name
          (Ast.print_li li)
      in
      let func =
        Option.value_exn ~message:func_error
          (lookup_function function_name current_mod)
      in
      let codegen_expr_map x =
        codegen_expr x (context, current_mod, builder, named_values)
      in
      let args = Array.of_list (List.map ~f:codegen_expr_map args) in
      build_call func args "" builder

let codegen_alias (name, alias) context =
  let ty = named_struct_type context name in
  let body = get_data_type alias context in
  struct_set_body ty (Array.create ~len:1 body) false

let codegen_shallow_fun { Tsr.Function.name; return_type; parameters; _ }
    context current_mod =
  let map_data_type (_, dt) = get_data_type dt context in
  let return_type = map_data_type ((), return_type) in
  let parameters = Array.of_list (List.map ~f:map_data_type parameters) in
  let function_ty = function_type return_type parameters in
  let _ = define_function name function_ty current_mod in
  ()

(** Duplicated from above. DELETE *)
let codegen_ext { Tsr.Extern.name; return_type; parameters; _ } context
    current_mod =
  let map_data_type dt = get_data_type dt context in
  let return_type = map_data_type return_type in
  let parameters = Array.of_list (List.map ~f:map_data_type parameters) in
  let function_ty = function_type return_type parameters in
  let func = declare_function name function_ty current_mod in
  set_linkage Linkage.External func

let codegen_fun func context current_mod =
  let { Tsr.Function.name; parameters; body; _ } = func in
  let func = Option.value_exn (lookup_function name current_mod) in
  let builder = builder_at_end context (entry_block func) in
  let named_values = Hashtbl.create (module String) in
  let generate_named_value name ty =
    let count =
      match Hashtbl.find named_values name with
      | Some (_, c) -> c + 1
      | None -> 0
    in
    let tagged_name = name ^ string_of_int count in
    let value = build_alloca ty tagged_name builder in
    Hashtbl.set named_values ~key:name ~data:(value, count);
    value
  in
  let argument_codegen (name, ty) value =
    let ty = get_data_type ty context in
    let location = generate_named_value name ty in
    let _ = build_store value location builder in
    ()
  in
  List.iter2_exn ~f:argument_codegen parameters (Array.to_list (params func));

  let basic_block_count = ref 1 in
  let generate_basic_block_name =
    let count = !basic_block_count in
    basic_block_count := count + 1;
    name ^ string_of_int count
  in
  let rec codegen_stmt stmt =
    match stmt with
    | Expression (expr, _) ->
        let _ =
          codegen_expr expr (context, current_mod, builder, named_values)
        in
        ()
    | Assignment (name, ty, expr, reassignment, li) ->
        let value =
          codegen_expr expr (context, current_mod, builder, named_values)
        in
        let location =
          if not reassignment then
            let ty = get_data_type ty context in
            generate_named_value name ty
          else
            let location = Hashtbl.find named_values name in
            let error =
              Printf.sprintf "no known variable %s in scope of (%s)" name
                (Ast.print_li li)
            in
            let location, _ = Option.value_exn ~message:error location in
            location
        in
        let _ = build_store value location builder in
        ()
    | Conditional (expr, body, repeated, _) ->
        let condition_bb =
          append_block context generate_basic_block_name func
        in
        let _ = build_br condition_bb builder in
        let taken_bb = append_block context generate_basic_block_name func in
        let not_taken_bb =
          append_block context generate_basic_block_name func
        in
        position_at_end condition_bb builder;
        (* should typecheck it's a bool during lowering *)
        let condition =
          codegen_expr expr (context, current_mod, builder, named_values)
        in
        let _ = build_cond_br condition taken_bb not_taken_bb builder in
        position_at_end taken_bb builder;
        codegen_code_block body (fun _ _ -> ());
        let _ =
          if repeated then build_br condition_bb builder
          else build_br not_taken_bb builder
        in
        position_at_end not_taken_bb builder
  and codegen_code_block (stmts, final_expr, _) f =
    List.iter ~f:codegen_stmt stmts;
    let final_value =
      Option.map
        ~f:(fun x ->
          codegen_expr x (context, current_mod, builder, named_values))
        final_expr
    in
    f final_value final_expr
  in
  let insert_ret value expr =
    let _ =
      match Option.value ~default:Void (Option.map ~f:expr_type expr) with
      | Void -> build_ret_void builder
      | _ ->
          build_ret
            (Option.value_exn
               ~message:"internal error value final expr not same optional"
               value)
            builder
    in
    ()
  in
  codegen_code_block body insert_ret

let generate_code name { Tsr.externs; functions; aliases } context =
  let _ = (externs, functions) in
  let current_mod = create_module context name in
  generate_operator_functions context current_mod;
  List.iter ~f:(fun x -> codegen_alias x context) aliases;
  List.iter ~f:(fun x -> codegen_ext x context current_mod) externs;
  List.iter ~f:(fun x -> codegen_shallow_fun x context current_mod) functions;
  List.iter ~f:(fun x -> codegen_fun x context current_mod) functions;
  current_mod
