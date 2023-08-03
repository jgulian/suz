open Core
open Environment
open Option
open General

let map_op_to_name op ty =
  match op with
  | Ast.Add _ -> "_add_" ^ Tsr.name_data_type ty
  | Ast.Sub _ -> "_sub_" ^ Tsr.name_data_type ty
  | Ast.Mul _ -> "_mul_" ^ Tsr.name_data_type ty
  | Ast.Equals _ -> "_equals_" ^ Tsr.name_data_type ty
  | Ast.NotEquals _ -> "_not_equals_" ^ Tsr.name_data_type ty
  | Ast.Less _ -> "_less_" ^ Tsr.name_data_type ty
  | Ast.Greater _ -> "_greater_" ^ Tsr.name_data_type ty

let rec fold_node env expr action =
  match action with
  | Ast.IndexAccess (i, li) -> (
      match Tsr.expr_type expr with
      | Tuple factor_tys -> (
          match List.nth factor_tys i with
          | Some dt ->
              Tsr.TupleAccess (expr, i, dt, li) (* these li aren't correct *)
          | None ->
              raise
                (Syntax_error
                   (Printf.sprintf "tuple access out of bounds (%s)"
                      (print_li li))))
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "can only access tuples (%s)" (print_li li))))
  | NamedAccess _ -> raise Unimplemented
  | DerefAccess (i, li) -> (
      match Tsr.expr_type expr with
      | Pointer dt -> Tsr.IndexDeref (expr, map_expr env i, dt, li)
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "can only dereference pointers (%s)"
                  (print_li li))))

and map_expr env expr =
  let map_expr_e = map_expr env in
  match expr with
  | Ast.Literal (v, dt, li) ->
      let error =
        Printf.sprintf "unable to infer type of literal at (%s)" (print_li li)
      in
      let dt = Option.value_exn ~message:error dt in
      Tsr.Literal (v, map_data_type env dt, li)
  | Ast.Variable (we, li) ->
      let source, path, _ = we in
      let error =
        Printf.sprintf "no variable %s in scope (%s)" source (print_li li)
      in
      let source_dt =
        Option.value_exn ~message:error
          (Hashtbl.find env.scoped_variables source)
      in
      let source = Tsr.Variable (source, source_dt, li) in
      List.fold path ~init:source ~f:(fold_node env)
  | Ast.Unary (expr, op, li) -> (
      let expr = map_expr_e expr in
      match op with Ast.Not _ -> Tsr.Call ("_not_bool", [ expr ], Tsr.Bool, li))
  | Ast.Binary (lhs, rhs, op, li) ->
      let lhs = map_expr_e lhs in
      let rhs = map_expr_e rhs in
      let ty = Tsr.op_return_type op (Tsr.expr_type lhs) in
      Tsr.Binary (lhs, rhs, op, ty, li)
  | Ast.Call (name, args, li) ->
      let args = List.map ~f:map_expr_e args in
      let error =
        Printf.sprintf "no function %s in scope (%s)" name (print_li li)
      in
      let func_return_type =
        Option.value_exn ~message:error (Hashtbl.find env.function_types name)
      in
      Tsr.Call (name, args, func_return_type, li)
  | Ast.TupleBuild (factors, li) ->
      let factors = List.map ~f:map_expr_e factors in
      let factor_tys = List.map ~f:Tsr.expr_type factors in
      let tuple_ty = Tsr.Tuple factor_tys in
      Tsr.TupleConstruction (factors, tuple_ty, li)
  | Ast.TupleAccess (expr, index, li) ->
      let expr = map_expr_e expr in
      let tuple_ty = Tsr.expr_type expr in
      let ty =
        match tuple_ty with
        | Tsr.Tuple factors -> List.nth factors index
        | _ -> None
      in
      let ty =
        Option.value_exn
          ~message:
            (Printf.sprintf "no index %d on tuple at (%s)" index (print_li li))
          ty
      in
      let hash = string_of_int (Tsr.DataType.hash tuple_ty) in
      Tsr.Call
        (Printf.sprintf "_tuple_access_%s_%d" hash index, [ expr ], ty, li)

let map_location env loc =
  match loc with
  | Ast.IndexAccess (i, li) -> Tsr.Access (i, li)
  | NamedAccess _ -> raise Unimplemented
  | DerefAccess (i, li) -> Deref (map_expr env i, li)

let rec map_stmt env stmt =
  let map_data_type_e = map_data_type env in
  let map_expr_e = map_expr env in
  match stmt with
  | Ast.Expression (expr, li) -> Tsr.Expression (map_expr_e expr, li)
  | Ast.Assignment (var, dt, expr, li) ->
      let expr = map_expr_e expr in
      let dt = map_data_type_e dt in
      let actual_data_type = Tsr.expr_type expr in
      if not (Tsr.equal_data_type dt actual_data_type) then
        raise
          (Syntax_error
             (Printf.sprintf
                "expected data type does not equal actual data type (%s)"
                (print_li li)))
      else Hashtbl.set env.scoped_variables ~key:var ~data:actual_data_type;
      Tsr.Assignment (var, [], actual_data_type, expr, false, li)
  | Ast.Reassignment (we, expr, li) ->
      let source, path, _ = we in
      let expr = map_expr_e expr in
      let path = List.map ~f:(map_location env) path in
      let data_type = Ast.Variable (we, li) |> map_expr_e |> Tsr.expr_type in
      let actual_data_type = Tsr.expr_type expr in
      if not (Tsr.equal_data_type data_type actual_data_type) then
        raise
          (Syntax_error
             (Printf.sprintf
                "expected data type does not equal actual data type (%s)"
                (print_li li)))
      else ();
      Tsr.Assignment (source, path, actual_data_type, expr, true, li)
  | Ast.If (expr, body, li) ->
      let expr = map_expr_e expr in
      let _ =
        match Tsr.expr_type expr with
        | Bool -> ()
        | _ ->
            raise
              (Type_error
                 (Printf.sprintf "expected bool condition for if statement (%s)"
                    (print_li li)))
      in
      Tsr.Conditional (expr, map_code_block env body, false, li)
  | Ast.While (expr, body, li) ->
      let expr = map_expr_e expr in
      let _ =
        match Tsr.expr_type expr with
        | Bool -> ()
        | _ ->
            raise
              (Type_error
                 (Printf.sprintf
                    "expected bool condition for while statement (%s)"
                    (print_li li)))
      in
      Tsr.Conditional (expr, map_code_block env body, true, li)

and map_code_block env (stmts, final_expr, li) =
  let stmts = List.map ~f:(map_stmt env) stmts in
  let final_expr = final_expr >>| map_expr env in
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
        List.map ~f:(fun (n, b, t) -> (n, b, map_data_type env t)) parameters
      in
      let env = clone_env env in
      List.iter
        ~f:(fun (n, _, t) -> Hashtbl.set env.scoped_variables ~key:n ~data:t)
        parameters;
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
  let externs = List.filter_map items ~f:(map_extern env) in
  let functions = List.filter_map items ~f:(map_function env) in
  let aliases = List.filter_map items ~f:(filter_alias env) in
  { Tsr.externs; functions; aliases }
