open Core
open Llvm
open Structure
open Tsr
open Data_type

let cartesian l l' f =
  List.iter ~f:(fun e -> List.iter ~f:(fun e' -> f e e') l') l

let generate_operator_functions context current_mod =
  let get_data_type_c = get_data_type context in
  let generate_operator_function op ty inst =
    let name = Lowering.map_op_to_name op ty in
    print_endline name;
    let return_ty = get_data_type_c (Lowering.op_return_type op ty) in
    let ty = get_data_type_c ty in
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
  let operations =
    [
      Ast.Add li;
      Ast.Sub li;
      Ast.Mul li;
      Ast.Equals li;
      Ast.NotEquals li;
      Ast.Greater li;
      Ast.Less li;
    ]
  in
  let types =
    [
      Numeric (Unsigned, 1);
      Numeric (Unsigned, 2);
      Numeric (Unsigned, 4);
      Numeric (Unsigned, 8);
      Numeric (Signed, 1);
      Numeric (Signed, 2);
      Numeric (Signed, 4);
      Numeric (Signed, 8);
      Numeric (Floating, 4);
      Numeric (Floating, 8);
    ]
  in
  let gen_inst op ty =
    match (op, ty) with
    | Ast.Add _, Numeric (Floating, _) -> build_fadd
    | Ast.Add _, _ -> build_add
    | Ast.Sub _, Numeric (Floating, _) -> build_fsub
    | Ast.Sub _, _ -> build_sub
    | Ast.Mul _, Numeric (Floating, _) -> build_fmul
    | Ast.Mul _, _ -> build_mul
    | Ast.Equals _, Numeric (Floating, _) -> build_fcmp Fcmp.Oeq
    | Ast.Equals _, _ -> build_icmp Icmp.Eq
    | Ast.NotEquals _, Numeric (Floating, _) -> build_fcmp Fcmp.One
    | Ast.NotEquals _, _ -> build_icmp Icmp.Ne
    | Ast.Less _, Numeric (Floating, _) -> build_fcmp Fcmp.Olt
    | Ast.Less _, Numeric (Signed, _) -> build_icmp Icmp.Slt
    | Ast.Less _, Numeric (Unsigned, _) -> build_icmp Icmp.Ult
    | Ast.Greater _, Numeric (Floating, _) -> build_fcmp Fcmp.Ogt
    | Ast.Greater _, Numeric (Signed, _) -> build_icmp Icmp.Sgt
    | Ast.Greater _, Numeric (Unsigned, _) -> build_icmp Icmp.Ugt
    | _, _ -> raise (Internal_issue "attempted to build unsupported type")
  in
  let gen op ty = generate_operator_function op ty (gen_inst op ty) in
  cartesian operations types gen
