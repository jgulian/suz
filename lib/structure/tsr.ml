open Core

type location_information = Ast.location_information
and numeric_format = Ast.numeric_format
and data_type = Void | Bool | Numeric of numeric_format
(* Instead of named type here, use ref of the defined struct *)

let equal_data_type a b =
  match (a, b) with
  | Void, Void -> true
  | Numeric a, Numeric b -> (
      match (a, b) with
      | Floating, Floating -> true
      | Signed, Signed -> true
      | Unsigned, Unsigned -> true
      | _ -> false)
  | _ -> false

let name_data_type a =
  match a with
  | Void -> "_void"
  | Bool -> "_bool"
  | Numeric a -> (
      match a with Floating -> "_f" | Signed -> "_s" | Unsigned -> "_u")

type expression =
  | VoidData of location_information
  | Literal of float * data_type * location_information
  | Variable of string * data_type * location_information
  | Call of string * expression list * data_type * location_information

let expr_type expr =
  match expr with
  | VoidData _ -> Void
  | Literal (_, dt, _) -> dt
  | Variable (_, dt, _) -> dt
  | Call (_, _, dt, _) -> dt

type statement =
  | Expression of expression * location_information
  | Assignment of string * data_type * expression * location_information
  | Conditional of expression * code_block * bool * location_information

and code_block = statement list * expression * data_type * location_information

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

type build_module = { externs : Extern.t list; functions : Function.t list }

let debug_print { externs; functions } =
  let print_dt dt =
    match dt with
    | Void -> "Void"
    | Bool -> "Bool"
    | Numeric a -> Printf.sprintf "Numeric (%s)" (Ast.print_nf a)
  in
  let print_ext { Extern.name; return_type; parameters; location } =
    let parameters =
      String.concat ~sep:", " (List.map ~f:print_dt parameters)
    in
    Printf.sprintf "Extern (%s, %s, %s, (%s))" name (print_dt return_type)
      parameters (Ast.print_li location)
  in
  let rec print_expr expr i =
    match expr with
    | VoidData li -> Printf.sprintf "VoidData (%s)" (Ast.print_li li)
    | Literal (v, dt, li) ->
        Printf.sprintf "Literal %f, %s, (%s)" v (print_dt dt) (Ast.print_li li)
    | Variable (name, dt, li) ->
        Printf.sprintf "Variable %s, %s, (%s)" name (print_dt dt)
          (Ast.print_li li)
    | Call (func, args, dt, li) ->
        let tabs = Ast.nt i in
        let args =
          String.concat ~sep:(",\n\t" ^ tabs)
            (List.map ~f:(fun x -> print_expr x (i + 1)) args)
        in
        Printf.sprintf "Call %s, %s, (\n%s\t%s\n%s), (%s)" func (print_dt dt)
          tabs args tabs (Ast.print_li li)
  in
  let rec print_stmt stmt i =
    match stmt with
    | Expression (expr, li) ->
        Printf.sprintf "Expression %s (%s)" (print_expr expr i)
          (Ast.print_li li)
    | Assignment (var, dt, expr, li) ->
        let tabs = Ast.nt i in
        Printf.sprintf "Assignment %s, %s, (%s), \n%s%s" var (print_dt dt)
          (Ast.print_li li) tabs
          (print_expr expr (i + 1))
    | Conditional (expr, body, repeated, li) ->
        let tabs = Ast.nt i in
        Printf.sprintf
          "Condition {\n\
           %sCondition: %s\n\
           %sBody: %s\n\
           %sRepeated: %b\n\
           %sLocationInfo: (%s) }" tabs
          (print_expr expr (i + 1))
          tabs
          (print_cb body (i + 1))
          tabs repeated tabs (Ast.print_li li)
  and print_cb (stmts, final_expr, dt, li) i =
    let tabs = Ast.nt i in
    let body =
      String.concat ~sep:(",\n\t" ^ tabs)
        (List.map ~f:(fun x -> print_stmt x (i + 1)) stmts)
    in
    let body = body ^ "\n\t" ^ tabs ^ print_expr final_expr (i + 1) in
    Printf.sprintf "CodeBlock %s, (%s), \n%s%s" (print_dt dt) (Ast.print_li li)
      tabs body
  in
  let print_func { Function.name; return_type; parameters; body; location } i =
    let tabs = Ast.nt i in
    let parameters =
      String.concat ~sep:", "
        (List.map ~f:(fun (n, dt) -> n ^ ": " ^ print_dt dt) parameters)
    in
    Printf.sprintf "Function %s, %s, %s, (%s), \n\t%s" name
      (print_dt return_type) parameters (Ast.print_li location)
      (tabs ^ print_cb body (i + 1))
  in
  let externs = String.concat ~sep:"\n" (List.map ~f:print_ext externs) in
  let functions =
    String.concat ~sep:"\n" (List.map ~f:(fun f -> print_func f 0) functions)
  in
  externs ^ "\n" ^ functions
