open Core

type location_information = {
  file : string;
  lines : int * int;
  columns : int * int;
}

type numeric_format = Floating | Signed | Unsigned

and data_type =
  | Void of location_information
  | Numeric of numeric_format * location_information

and binary_operation =
  | Add of location_information
  | Sub of location_information

and expression =
  | VoidData of location_information
  | Literal of float * data_type option * location_information
  | Variable of string * location_information
  | Binary of expression * expression * binary_operation * location_information
  | Call of string * expression list * location_information

and statement =
  | Expression of expression * location_information
  | Assignment of string * data_type * expression * location_information

and code_block = statement list * expression * location_information

module Extern = struct
  type t = {
    name : string;
    return_type : data_type;
    parameters : data_type list;
    location : location_information;
  }
end

module Function = struct
  type t = {
    name : string;
    return_type : data_type;
    parameters : (string * data_type) list;
    body : code_block;
    location : location_information;
  }
end

type item =
  | Extern of Extern.t * location_information
  | Function of Function.t * location_information

let debug_print items =
  let print_li li =
    let a, b = li.lines in
    let c, d = li.columns in
    Printf.sprintf "%s, %d-%d, %d-%d" li.file a b c d
  in
  let print_nf f =
    match f with
    | Floating -> "Floating"
    | Signed -> "Signed"
    | Unsigned -> "Unsigned"
  in
  let print_dt dt =
    match dt with
    | Void li -> Printf.sprintf "Void (%s)" (print_li li)
    | Numeric (f, li) ->
        Printf.sprintf "Numeric (%s, (%s))" (print_nf f) (print_li li)
  in
  let print_dt_l dt_l = String.concat ~sep:", " (List.map ~f:print_dt dt_l) in
  let print_bo bo =
    match bo with
    | Add li -> Printf.sprintf "Add (%s)" (print_li li)
    | Sub li -> Printf.sprintf "Sub (%s)" (print_li li)
  in
  let rec print_expr expr =
    match expr with
    | VoidData li -> Printf.sprintf "VoidData (%s)" (print_li li)
    | Literal (v, dt, li) ->
        Printf.sprintf "Literal (%f, %s, (%s))" v
          (Option.value ~default:"None" (Option.map ~f:print_dt dt))
          (print_li li)
    | Variable (n, li) -> Printf.sprintf "Variable (%s, (%s))" n (print_li li)
    | Binary (lhs, rhs, bo, li) ->
        Printf.sprintf "Binary (%s, %s, %s, (%s))" (print_expr lhs)
          (print_expr rhs) (print_bo bo) (print_li li)
    | Call (name, args, li) ->
        Printf.sprintf "Call (%s, (%s), (%s))" name (print_expr_list args)
          (print_li li)
  and print_expr_list expr_list =
    String.concat ~sep:", " (List.map ~f:print_expr expr_list)
  in
  let print_stmt stmt =
    match stmt with
    | Expression (expr, li) ->
        Printf.sprintf "Expression (%s, %s)" (print_expr expr) (print_li li)
    | Assignment (name, dt, expr, li) ->
        Printf.sprintf "Assignment (%s, %s, %s, %s)" name (print_dt dt)
          (print_expr expr) (print_li li)
  in
  let rec nt n = if n = 0 then "" else "\t" ^ nt (n - 1) in
  let print_cb (stmts, expr, li) i =
    let print_stmt_elem stmt =
      Printf.sprintf "%s%s" (nt (i + 1)) (print_stmt stmt)
    in
    let stmts_end = if List.is_empty stmts then "" else "\n" in
    let stmts = String.concat ~sep:"\n" (List.map ~f:print_stmt_elem stmts) in
    Printf.sprintf
      "CodeBlock {\n\
       %sStatements:\n\
       %s\
       %sFinalExpression: %s\n\
       %sLocationInfo: (%s) }" (nt i)
      (stmts ^ stmts_end) (nt i) (print_expr expr) (nt i) (print_li li)
  in
  let print_ext (ext : Extern.t) i =
    Printf.sprintf
      "Extern {\n\
       %sName: %s\n\
       %sReturnType: %s\n\
       %sArguments: %s\n\
       %sLocationInfo: (%s) }" (nt i) ext.name (nt i) (print_dt ext.return_type)
      (nt i)
      (print_dt_l ext.parameters)
      (nt i) (print_li ext.location)
  in
  let print_args dt_l =
    let m (name, dt) = name ^ print_dt dt in
    String.concat ~sep:", " (List.map ~f:m dt_l)
  in
  let print_fun (f : Function.t) i =
    Printf.sprintf
      "Function {\n\
       %sName: %s\n\
       %sReturnType: %s\n\
       %sArguments: %s\n\
       %sBlock: %s\n\
       %sLocationInfo: %s }" (nt i) f.name (nt i) (print_dt f.return_type)
      (nt i) (print_args f.parameters) (nt i)
      (print_cb f.body (i + 1))
      (nt i) (print_li f.location)
  in
  let print_item it i =
    match it with
    | Extern (ext, _) -> print_ext ext (i + 1)
    | Function (f, _) -> print_fun f (i + 1)
  in
  String.concat ~sep:"\n" (List.map ~f:(fun i -> print_item i 0) items)

let parse_numeric_type s li =
  (*This function should never cause as long as the argument is a well
     formatted string with regard to the lexer *)
  let ty =
    match String.get s 0 with
    | 'f' -> Floating
    | 's' -> Signed
    | 'u' -> Unsigned
    | _ -> raise (Invalid_argument "not well formatted numeric literal")
  in
  Numeric (ty, li)

let parse_numeric_literal s li =
  (* An exception should not be raised here because the split should always
     be able to happen. All strings passed into this function are assumed to
     have an '_' seperating the number and the type *)
  let value, ty = String.lsplit2_exn s ~on:'_' in
  let first_col, last_col = li.columns in
  let first_col = first_col + String.length value in
  let sub_li =
    { file = li.file; lines = li.lines; columns = (first_col, last_col) }
  in
  let value = float_of_string value in
  Literal (value, Some (parse_numeric_type ty sub_li), li)

let li (p : Lexing.position * Lexing.position) =
  let a, b = p in
  {
    file = a.pos_fname;
    lines = (a.pos_lnum, b.pos_lnum);
    columns = (a.pos_cnum - a.pos_bol + 1, b.pos_cnum - b.pos_bol + 1);
  }
