open Structure
open Llvm
open Tsr
open Core

exception Unexpected_type of string
exception Internal_issue of string

let define_void_type context =
  let void_ty = named_struct_type context "_void_type" in
  struct_set_body void_ty (Array.of_list []) false

let get_void_value context = const_struct context (Array.of_list [])

let get_data_type dt context =
  match dt with
  | Void -> void_type context
  | Bool -> i1_type context
  | Numeric format -> (
      match format with
      | Floating -> double_type context
      | Signed -> i64_type context
      | Unsigned -> i64_type context)

let generate_operator_functions context current_mod =
  let name = "_add__u" in
  let dt = get_data_type (Numeric Unsigned) context in
  let function_ty = function_type dt (Array.of_list [ dt; dt ]) in
  let func = define_function name function_ty current_mod in
  let builder = builder_at_end context (entry_block func) in
  let a, b =
    match Array.to_list (params func) with
    | [ a; b ] -> (a, b)
    | _ ->
        raise
          (Internal_issue
             ("incorrect number of arguments in operator function " ^ name))
  in
  let _ = build_ret (build_add a b "" builder) builder in
  (* Split *)
  let name = "_sub__u" in
  let dt = get_data_type (Numeric Unsigned) context in
  let function_ty = function_type dt (Array.of_list [ dt; dt ]) in
  let func = define_function name function_ty current_mod in
  let builder = builder_at_end context (entry_block func) in
  let a, b =
    match Array.to_list (params func) with
    | [ a; b ] -> (a, b)
    | _ ->
        raise
          (Internal_issue
             ("incorrect number of arguments in operator function " ^ name))
  in
  let _ = build_ret (build_sub a b "" builder) builder in
  (*Split *)
  let name = "_equals__u" in
  let dt = get_data_type (Numeric Unsigned) context in
  let return_type = get_data_type Bool context in
  let function_ty = function_type return_type (Array.of_list [ dt; dt ]) in
  let func = define_function name function_ty current_mod in
  let builder = builder_at_end context (entry_block func) in
  let a, b =
    match Array.to_list (params func) with
    | [ a; b ] -> (a, b)
    | _ ->
        raise
          (Internal_issue
             ("incorrect number of arguments in operator function " ^ name))
  in
  let _ = build_ret (build_icmp Icmp.Eq a b "" builder) builder in
  ()

let rec codegen_expr expr build_context =
  let context, current_mod, builder, named_values = build_context in
  match expr with
  | VoidData _ -> get_void_value context
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
      let value = Hashtbl.find named_values name in
      Option.value_exn ~message:error value
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
  let { Tsr.Function.name; parameters; body; location; _ } = func in
  let func = Option.value_exn (lookup_function name current_mod) in
  let builder = builder_at_end context (entry_block func) in
  let arguments =
    List.map2
      ~f:(fun (name, _) value -> (name, value))
      parameters
      (Array.to_list (params func))
  in
  let arguments =
    match arguments with
    | Ok args -> args
    | Unequal_lengths ->
        raise
          (Internal_issue
             (Printf.sprintf
                "unequal parameter lengths between function and itself (%s)"
                (Ast.print_li location)))
  in
  let named_values = Hashtbl.of_alist_or_error (module String) arguments in
  let named_value_error =
    Printf.sprintf "duplicate parameter name in function %s at (%s)" name
      (Ast.print_li location)
  in
  let named_values =
    Option.value_exn ~message:named_value_error (Result.ok named_values)
  in
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
    | Assignment (name, _, expr, _) ->
        let value =
          codegen_expr expr (context, current_mod, builder, named_values)
        in
        Hashtbl.set named_values ~key:name ~data:value
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
        let _ =
          if repeated then
            (* If statements and while loops have the opposite taken semantics.
               This could be fixed in the future by inserting a not in the Ast. *)
            build_cond_br condition not_taken_bb taken_bb builder
          else build_cond_br condition taken_bb not_taken_bb builder
        in
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

let generate_code name { Tsr.externs; functions } context =
  let _ = (externs, functions) in
  let current_mod = create_module context name in
  define_void_type context;
  generate_operator_functions context current_mod;
  List.iter ~f:(fun x -> codegen_ext x context current_mod) externs;
  List.iter ~f:(fun x -> codegen_shallow_fun x context current_mod) functions;
  List.iter ~f:(fun x -> codegen_fun x context current_mod) functions;
  current_mod
