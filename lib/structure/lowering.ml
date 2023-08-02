open Core
open Environment

exception Syntax_error of string
exception Type_error of string

let map_op_to_name op ty =
  match op with
  | Ast.Add _ -> "_add_" ^ Tsr.name_data_type ty
  | Ast.Sub _ -> "_sub_" ^ Tsr.name_data_type ty
  | Ast.Mul _ -> "_mul_" ^ Tsr.name_data_type ty
  | Ast.Equals _ -> "_equals_" ^ Tsr.name_data_type ty
  | Ast.NotEquals _ -> "_not_equals_" ^ Tsr.name_data_type ty
  | Ast.Less _ -> "_less_" ^ Tsr.name_data_type ty
  | Ast.Greater _ -> "_greater_" ^ Tsr.name_data_type ty

let op_return_type op ty =
  match op with
  | Ast.Add _ | Ast.Sub _ | Ast.Mul _ -> ty
  | Ast.Equals _ | Ast.NotEquals _ | Ast.Less _ | Ast.Greater _ -> Tsr.Bool

let rec map_expr env expr =
  let map_expr_e = map_expr env in
  match expr with
  | Ast.Literal (v, dt, li) ->
      let error =
        Printf.sprintf "unable to infer type of literal at (%s)"
          (Ast.print_li li)
      in
      let dt = Option.value_exn ~message:error dt in
      Tsr.Literal (v, map_data_type env dt, li)
  | Ast.Variable (name, li) ->
      let error =
        Printf.sprintf "no variable %s in scope (%s)" name (Ast.print_li li)
      in
      let dt =
        Option.value_exn ~message:error (Hashtbl.find env.scoped_variables name)
      in
      Tsr.Variable (name, dt, li)
  | Ast.Binary (lhs, rhs, op, li) ->
      let lhs = map_expr_e lhs in
      let rhs = map_expr_e rhs in
      let ty = op_return_type op (Tsr.expr_type lhs) in
      Tsr.Call (map_op_to_name op (Tsr.expr_type lhs), [ lhs; rhs ], ty, li)
  | Ast.Call (name, args, li) ->
      let args = List.map ~f:map_expr_e args in
      let error =
        Printf.sprintf "no function %s in scope (%s)" name (Ast.print_li li)
      in
      let func_return_type =
        Option.value_exn ~message:error (Hashtbl.find env.function_types name)
      in
      Tsr.Call (name, args, func_return_type, li)
  | TupleAccess (expr, index, li) ->
      let expr = map_expr_e expr in 
      let tuple_ty = Tsr.expr_type expr in
      Hash_set.add env.tuples tuple_ty;
      let ty = (match tuple_ty with 
      | Tsr.Tuple factors -> List.nth factors index | _ -> None) in 
      let ty = Option.value_exn ~message:(Printf.sprintf "no index %d on tuple at (%s)" index (Ast.print_li li)) ty in
      Tsr.Call ("_tuple_access_" ^ (string_of_int (Tsr.DataType.hash tuple_ty)), [expr], ty, li)

let rec map_stmt env stmt =
  let map_data_type_e = map_data_type env in
  let map_expr = map_expr env in
  match stmt with
  | Ast.Expression (expr, li) -> Tsr.Expression (map_expr expr, li)
  | Ast.Assignment (var, dt, expr, li) ->
      let expr = map_expr expr in
      let dt = map_data_type_e dt in
      let actual_data_type = Tsr.expr_type expr in
      if not (Tsr.equal_data_type dt actual_data_type) then
        raise
          (Syntax_error
             (Printf.sprintf
                "expected data type does not equal actual data type (%s)"
                (Ast.print_li li)))
      else Hashtbl.set env.scoped_variables ~key:var ~data:actual_data_type;
      Tsr.Assignment (var, actual_data_type, expr, false, li)
  | Ast.Reassignment (var, expr, li) ->
      let expr = map_expr expr in
      let data_type = Hashtbl.find env.scoped_variables var in
      let not_found_error =
        Printf.sprintf "no variable %s in scope (%s)" var (Ast.print_li li)
      in
      let data_type = Option.value_exn ~message:not_found_error data_type in
      let actual_data_type = Tsr.expr_type expr in
      if not (Tsr.equal_data_type data_type actual_data_type) then
        raise
          (Syntax_error
             (Printf.sprintf
                "expected data type does not equal actual data type (%s)"
                (Ast.print_li li)))
      else ();
      Tsr.Assignment (var, actual_data_type, expr, true, li)
  | Ast.If (expr, body, li) ->
      let expr = map_expr expr in
      let _ =
        match Tsr.expr_type expr with
        | Bool -> ()
        | _ ->
            raise
              (Type_error
                 (Printf.sprintf "expected bool condition for if statement (%s)"
                    (Ast.print_li li)))
      in
      Tsr.Conditional (expr, map_code_block env body, false, li)
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
      Tsr.Conditional (expr, map_code_block env body, true, li)

and map_code_block env (stmts, final_expr, li) =
  let stmts = List.map ~f:(map_stmt env) stmts in
  let final_expr = Option.map ~f:(map_expr env) final_expr in
  (stmts, final_expr, li)

let map_extern env i =
  match i with
  | Ast.Extern ({ name; return_type; parameters; location }, _) ->
      Some
        {
          Tsr.Extern.return_type = map_data_type env return_type;
          parameters = List.map ~f:(map_data_type env) parameters;
          name;
          location;
        }
  | _ -> None

let map_function env i =
  match i with
  | Ast.Function ({ name; return_type; parameters; body; location }, _) ->
      let parameters =
        List.map ~f:(fun (n, t) -> (n, map_data_type env t)) parameters
      in
      Some
        {
          Tsr.Function.return_type = map_data_type env return_type;
          parameters;
          name;
          location;
          body = map_code_block env body;
        }
  | _ -> None

let filter_alias env i =
  match i with
  | Ast.Type (name, alias, _) -> Some (name, map_data_type env alias)
  | _ -> None

let lower items =
  let env = build_environment items in
  {
    Tsr.externs = List.filter_map items ~f:(map_extern env);
    functions = List.filter_map items ~f:(map_function env);
    aliases = List.filter_map items ~f:(filter_alias env);
  }
