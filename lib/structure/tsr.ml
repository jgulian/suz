open Core

type location_information = Ast.location_information
and numeric_format = Ast.numeric_format

let compare_numeric_format a b =
  match (a, b) with
  | Ast.Floating, Ast.Floating -> 0
  | _, Ast.Floating -> -1
  | Ast.Floating, _ -> 1
  | Ast.Signed, Ast.Signed -> 0
  | _, Ast.Signed -> -1
  | Ast.Signed, _ -> 1
  | Ast.Unsigned, Ast.Unsigned -> 0

type data_type =
  | Void
  | Bool
  | Numeric of numeric_format
  | Tuple of data_type list
  | Named of string * data_type

module DataType = struct
  type t = data_type

  let rec compare a b =
    match (a, b) with
    | Void, Void -> 0
    | Void, _ -> 1
    | _, Void -> -1
    | Bool, Bool -> 0
    | Bool, _ -> 1
    | _, Bool -> -1
    | Numeric a, Numeric b -> compare_numeric_format a b
    | Numeric _, _ -> 1
    | _, Numeric _ -> -1
    | Tuple a, Tuple b ->
        if List.length a < List.length b then -1
        else if List.length a > List.length b then 1
        else
          let li =
            List.map2_exn a b ~f:compare |> List.filter ~f:(fun x -> x <> 0)
          in
          List.nth li 0 |> Option.value ~default:0
    | Tuple _, _ -> 1
    | _, Tuple _ -> -1
    | Named (a_n, a_dt), Named (b_n, b_dt) ->
        let name_compare = String.compare a_n b_n in
        if name_compare = 0 then compare a_dt b_dt else name_compare

  let rec sexp_of_t a =
    match a with 
      | Void -> Sexp.Atom "Void"
      | Bool -> Sexp.Atom "Bool"
      | Numeric format ->
        (match format with 
          | Floating -> Sexp.Atom "Floating"
          | Signed -> Sexp.Atom "Signed"
          | Unsigned -> Sexp.Atom "Unsigned")
      | Tuple li -> List.sexp_of_t sexp_of_t li
      | Named (name, alias) -> Sexp.List [Sexp.Atom name; sexp_of_t alias]

  let hash a = 
    sexp_of_t a |> Sexp.hash 
end

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

let rec name_data_type a =
  match a with
  | Void -> "_void"
  | Bool -> "_bool"
  | Numeric a -> (
      match a with Floating -> "_f" | Signed -> "_s" | Unsigned -> "_u")
  | Tuple f -> "_tuple" ^ String.concat ~sep:"_" (List.map ~f:name_data_type f)
  | Named (n, _) -> n

type expression =
  | Literal of float * data_type * location_information
  | Variable of string * data_type * location_information
  | Call of string * expression list * data_type * location_information

let expr_type expr =
  match expr with
  | Literal (_, dt, _) -> dt
  | Variable (_, dt, _) -> dt
  | Call (_, _, dt, _) -> dt

type statement =
  | Expression of expression * location_information
  | Assignment of string * data_type * expression * bool * location_information
  | Conditional of expression * code_block * bool * location_information

and code_block = statement list * expression option * location_information

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

type build_module = {
  externs : Extern.t list;
  functions : Function.t list;
  aliases : (string * data_type) list;
}

let debug_print { externs; functions; aliases } =
  let rec print_dt dt =
    match dt with
    | Void -> "Void"
    | Bool -> "Bool"
    | Numeric a -> Printf.sprintf "Numeric (%s)" (Ast.print_nf a)
    | Tuple factors ->
        let factors = String.concat ~sep:", " (List.map ~f:print_dt factors) in
        Printf.sprintf "Numeric (%s)" factors
    | Named (name, dt) -> Printf.sprintf "Named (%s, %s)" name (print_dt dt)
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
    | Assignment (var, dt, expr, reassignment, li) ->
        let tabs = Ast.nt i in
        Printf.sprintf "Assignment %s, %s, %b, (%s), \n%s%s" var (print_dt dt)
          reassignment (Ast.print_li li) tabs
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
  and print_cb (stmts, final_expr, li) i =
    let tabs = Ast.nt i in
    let body =
      String.concat ~sep:(",\n\t" ^ tabs)
        (List.map ~f:(fun x -> print_stmt x (i + 1)) stmts)
    in
    let final_expr =
      Option.value ~default:"None"
        (Option.map ~f:(fun x -> print_expr x (i + 1)) final_expr)
    in
    let body = body ^ "\n\t" ^ tabs ^ final_expr in
    Printf.sprintf "CodeBlock (%s), \n%s%s" (Ast.print_li li) tabs body
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
  let print_alias (name, alias) =
    Printf.sprintf "Alias %s = (%s)" name (print_dt alias)
  in
  let aliases = String.concat ~sep:"\n" (List.map ~f:print_alias aliases) in
  externs ^ "\n" ^ functions ^ "\n" ^ aliases
