open Structure
open Llvm
open Tsr
open Core
open Data_type
open Operators
open General

let rec codegen_expr build_context expr =
  let codegen_expr_r = codegen_expr build_context in
  let context, current_mod, builder, named_values = build_context in
  let get_data_type_c = get_data_type context in
  match expr with
  | Literal (v, dt, li) -> (
      match dt with
      | Numeric (format, _) -> (
        let ty = get_data_type_c dt in
          let location = build_alloca ty "" builder in
          let _ = match format with
          | Floating -> (const_float ty v |> build_store) location builder
          | Signed | Unsigned -> (const_int ty (int_of_float v) |> build_store) location builder
        in
        location
          ) 
      | _ ->
          raise
            (Unexpected_type
               (Printf.sprintf "unexpected type of literal at (%s)"
                  (print_li li))))
  | Variable (name, _, li) ->
      let error =
        Printf.sprintf "unknown variable reference %s at (%s)" name
          (print_li li)
      in
      let pointer = Hashtbl.find named_values name in
      let pointer, _ = Option.value_exn ~message:error pointer in
      pointer
  | Unary (expr, op, _, _) -> 
    let expr = codegen_expr_r expr in 
    let expr = build_load expr "" builder in
    begin
      match op with 
      | Not -> build_not expr "" builder
    end
  | Call (function_name, args, dt, li) ->
      let func_error =
        Printf.sprintf "unknown function reference %s at (%s)" function_name
          (print_li li)
      in
      let func =
        Option.value_exn ~message:func_error
          (lookup_function function_name current_mod)
      in
      let map_arg arg ty = 
        match classify_type ty with 
          | Pointer -> codegen_expr_r arg
          | _ -> (codegen_expr_r arg |> build_load) "" builder
      in
      let args = List.map2 ~f:map_arg args (type_of func|> return_type  |> param_types |> Array.to_list) in
      let args = match args with 
        | Ok args -> args 
        | _ -> 
          raise (Internal_issue ("function parameters and args of unequal length at " ^ (print_li li)))
    in
      let value = build_call func (Array.of_list args) "" builder in 
      begin
        match dt with 
        | Void -> value
        | _ -> let ty = get_data_type_c dt in 
        let location = build_alloca ty "" builder in 
        let _ = build_store value location builder in 
        location 
      end
  | Binary (lhs, rhs, op, dt, _) ->
      let ty = expr_type lhs in
      let lhs = codegen_expr_r lhs in
      let lhs = build_load lhs "" builder in
      let rhs = codegen_expr_r rhs in
      let rhs = build_load rhs "" builder in
      let value = (codegen_binary_instruction op ty) lhs rhs "" builder in 
      let ty = get_data_type_c dt in
      let location = build_alloca ty "" builder in 
      let _ = build_store value location builder in 
      location
  | TupleConstruction (factors, _, _) ->
      let factors = List.map ~f:codegen_expr_r factors in
      let factors = List.map ~f:(fun x -> build_load x "" builder) factors in
      let factor_tys = List.map ~f:type_of factors |> Array.of_list in
      let ty = struct_type context factor_tys in
      let location = build_alloca ty "" builder in
      let store_factor index factor =
        let pointer = build_struct_gep location index "" builder in
        let _ = build_store factor pointer builder in
        index + 1
      in
      let _ = List.fold factors ~init:0 ~f:store_factor in
      location
  | TupleAccess (expr, index, _, _) ->
      let location = codegen_expr_r expr in
      build_struct_gep location index "" builder
  | IndexDeref (expr, index, _, _) ->
      let expr = codegen_expr_r expr in
      let index = codegen_expr_r index in
      let index = build_load index "" builder in 
      (build_gep expr (Array.of_list [ index ]) "" builder |> build_load) "" builder

let codegen_alias (name, alias) context =
  let ty = named_struct_type context name in
  let body = get_data_type context alias in
  struct_set_body ty (Array.create ~len:1 body) false

let codegen_shallow_fun { Tsr.Function.name; return_type; parameters; _ }
    context current_mod =
    let map_data_type (_, ref, dt) = 
      let ty = get_data_type context dt in
      if ref then pointer_type ty else ty in
  let return_type = get_data_type context return_type in
  let parameters = Array.of_list (List.map ~f:map_data_type parameters) in
  let function_ty = function_type return_type parameters in
  let _ = define_function name function_ty current_mod in
  ()

(** Duplicated from above. DELETE *)
let codegen_ext { Tsr.Extern.name; return_type; parameters; _ } context
    current_mod =
  let map_data_type dt = get_data_type context dt in
  let return_type = map_data_type return_type in
  let parameters = Array.of_list (List.map ~f:map_data_type parameters) in
  let function_ty = function_type return_type parameters in
  let func = declare_function name function_ty current_mod in
  set_linkage Linkage.External func

let resolve_location build_context source path li =
  let _, _, builder, named_values = build_context in
  let location = Hashtbl.find named_values source in
  let error =
    Printf.sprintf "no known variable %s in scope of (%s)" source (print_li li)
  in
  let location, _ = Option.value_exn ~message:error location in
  let map_location value action =
    match action with
    | Access (i, _) -> build_struct_gep value i "" builder
    | Deref (i, _) ->
        let value = build_load value "" builder in 
        let value_storage = build_alloca (type_of value) "" builder in
        let _ = build_store value value_storage builder in
        let value = value_storage in
        let value = build_load value "" builder in
        let indices = Array.of_list [ codegen_expr build_context i ] in
        let indices = Array.map ~f:(fun x -> build_load x "" builder) indices in 
        build_gep value indices "" builder
  in
  if List.is_empty path then location
  else List.fold ~init:location ~f:map_location path

let codegen_fun func context current_mod =
  let { Tsr.Function.name; parameters; body; _ } = func in
  let func = Option.value_exn (lookup_function name current_mod) in
  let builder = builder_at_end context (entry_block func) in
  let named_values = Hashtbl.create (module String) in
  let generate_named_value name ref ty =
    let count =
      match Hashtbl.find named_values name with
      | Some (_, c) -> c + 1
      | None -> 0
    in
    let tagged_name = name ^ string_of_int count in
    match ref with 
      | Some value -> 
        Hashtbl.set named_values ~key:name ~data:(value, count);
        value
      | None ->
        let value = build_alloca ty tagged_name builder in
        Hashtbl.set named_values ~key:name ~data:(value, count);
        value
  in
  let argument_codegen (name, ref, ty) value =
    let ty = get_data_type context ty in
    let ref = if ref then Some value else None in 
    let location = generate_named_value name ref ty in 
    if Option.is_none ref then let _ = build_store value location builder in () else ()
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
          codegen_expr (context, current_mod, builder, named_values) expr
        in
        ()
    | Assignment (name, path, ty, expr, reassignment, li) ->
        let value =
          codegen_expr (context, current_mod, builder, named_values) expr
        in
        let location =
          if not reassignment then
            let ty = get_data_type context ty in
            generate_named_value name None ty
          else
            resolve_location
              (context, current_mod, builder, named_values)
              name path li
        in
        let value = build_load value "" builder in 
        let _ = build_store value location builder in ()
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
          codegen_expr (context, current_mod, builder, named_values) expr
        in
        let condition = build_load condition "" builder in 
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
          codegen_expr (context, current_mod, builder, named_values) x)
        final_expr
    in
    f final_value final_expr
  in
  let insert_ret value expr =
    let _ =
      match Option.value ~default:Void (Option.map ~f:expr_type expr) with
      | Void -> build_ret_void builder
      | _ ->
        let value = Option.value_exn
        ~message:"internal error value final expr not same optional"
        value in 
        let value = build_load value "" builder in 
        build_ret value builder
    in
    ()
  in
  codegen_code_block body insert_ret

let generate_code name { Tsr.externs; functions; aliases } context =
  let _ = (externs, functions) in
  let current_mod = create_module context name in
  List.iter ~f:(fun x -> codegen_alias x context) aliases;
  List.iter ~f:(fun x -> codegen_ext x context current_mod) externs;
  List.iter ~f:(fun x -> codegen_shallow_fun x context current_mod) functions;
  List.iter ~f:(fun x -> codegen_fun x context current_mod) functions;
  current_mod
