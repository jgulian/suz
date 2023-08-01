open Core

exception Syntax_error of string
exception Type_error of string

let map_data_type dt =
  match dt with
  | Ast.Void _ -> Tsr.Void
  | Ast.Bool _ -> Tsr.Bool
  | Ast.Numeric (format, _) -> Tsr.Numeric format

let map_op_to_name op ty =
  match op with
  | Ast.Add _ -> "_add_" ^ Tsr.name_data_type ty
  | Ast.Sub _ -> "_sub_" ^ Tsr.name_data_type ty
  | Ast.Equals _ -> "_equals_" ^ Tsr.name_data_type ty

let parameters_to_scope (outer_scope : (string * Tsr.data_type) list) li =
  let named_values = Hashtbl.of_alist_or_error (module String) outer_scope in
  let error =
    Printf.sprintf "duplicate argument name in scope (%s)" (Ast.print_li li)
  in
  Option.value_exn ~message:error (Result.ok named_values)

let rec map_code_block (stmts, final_expr, li)
    (named_values : (string, Tsr.data_type) Hashtbl.t) function_types =
  let rec map_expr expr =
    match expr with
    | Ast.VoidData li -> Tsr.VoidData li
    | Ast.Literal (v, dt, li) ->
        let error =
          Printf.sprintf "unable to infer type of literal at (%s)"
            (Ast.print_li li)
        in
        let dt = Option.value_exn ~message:error dt in
        Tsr.Literal (v, map_data_type dt, li)
    | Ast.Variable (name, li) ->
        let error =
          Printf.sprintf "no variable %s in scope (%s)" name (Ast.print_li li)
        in
        let dt =
          Option.value_exn ~message:error (Hashtbl.find named_values name)
        in
        Tsr.Variable (name, dt, li)
    | Ast.Binary (lhs, rhs, op, li) ->
        let lhs = map_expr lhs in
        let rhs = map_expr rhs in
        let ty =
          match op with Ast.Equals _ -> Tsr.Bool | _ -> Tsr.expr_type lhs
        in
        Tsr.Call (map_op_to_name op (Tsr.expr_type lhs), [ lhs; rhs ], ty, li)
    | Ast.Call (name, args, li) ->
        let args = List.map ~f:map_expr args in
        let error =
          Printf.sprintf "no function %s in scope (%s)" name (Ast.print_li li)
        in
        let func_return_type =
          Option.value_exn ~message:error (Hashtbl.find function_types name)
        in
        Tsr.Call (name, args, func_return_type, li)
  in
  let map_stmt stmt =
    match stmt with
    | Ast.Expression (expr, li) -> Tsr.Expression (map_expr expr, li)
    | Ast.Assignment (var, dt, expr, li) ->
        let expr = map_expr expr in
        let dt = map_data_type dt in
        let actual_data_type = Tsr.expr_type expr in
        if not (Tsr.equal_data_type dt actual_data_type) then
          raise
            (Syntax_error
               (Printf.sprintf
                  "expected data type does not equal actual data type (%s)"
                  (Ast.print_li li)))
        else Hashtbl.set named_values ~key:var ~data:actual_data_type;
        Tsr.Assignment (var, actual_data_type, expr, li)
    | Ast.If (expr, body, li) ->
        let expr = map_expr expr in
        let _ =
          match Tsr.expr_type expr with
          | Bool -> ()
          | _ ->
              raise
                (Type_error
                   (Printf.sprintf
                      "expected bool condition for if statement (%s)"
                      (Ast.print_li li)))
        in
        Tsr.Conditional
          (expr, map_code_block body named_values function_types, false, li)
    | Ast.While (expr, body, li) ->
        let expr = map_expr expr in
        let _ =
          match Tsr.expr_type expr with
          | Bool -> ()
          | _ ->
              raise
                (Type_error
                   (Printf.sprintf
                      "expected bool condition for while statement (%s)"
                      (Ast.print_li li)))
        in
        Tsr.Conditional
          (expr, map_code_block body named_values function_types, true, li)
  in
  let stmts = List.map ~f:map_stmt stmts in
  let final_expr = Option.map ~f:map_expr final_expr in
  (stmts, final_expr, li)

let map_extern i =
  match i with
  | Ast.Extern ({ name; return_type; parameters; location }, _) ->
      Some
        {
          Tsr.Extern.return_type = map_data_type return_type;
          parameters = List.map ~f:map_data_type parameters;
          name;
          location;
        }
  | _ -> None

let map_function i function_types =
  match i with
  | Ast.Function ({ name; return_type; parameters; body; location }, _) ->
      let parameters =
        List.map ~f:(fun (n, t) -> (n, map_data_type t)) parameters
      in
      Some
        {
          Tsr.Function.return_type = map_data_type return_type;
          parameters;
          name;
          location;
          body =
            map_code_block body
              (parameters_to_scope parameters location)
              function_types;
        }
  | _ -> None

let collect_function_types items =
  let filter_functions i =
    match i with
    | Ast.Function ({ name; return_type; _ }, _) ->
        Some (name, map_data_type return_type)
    | Ast.Extern ({ name; return_type; _ }, _) ->
        Some (name, map_data_type return_type)
  in
  let functions = List.filter_map ~f:filter_functions items in
  let table = Hashtbl.of_alist_or_error (module String) functions in
  Option.value_exn ~message:"duplicate function in module" (Result.ok table)

let lower items =
  let function_types = collect_function_types items in
  {
    Tsr.externs = List.filter_map ~f:map_extern items;
    functions =
      List.filter_map ~f:(fun x -> map_function x function_types) items;
  }
