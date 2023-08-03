open Core

type location_information = Ast.location_information
and numeric_format = Ast.numeric_format

type data_type =
  | Void
  | Bool
  | Numeric of numeric_format * int
  | Tuple of data_type list
  | Pointer of data_type
  | Named of string * data_type

  let op_return_type op ty =
    match op with
    | Ast.Add _ | Sub _ | Mul _ -> ty
    | Equals _ | NotEquals _ | Less _ | Greater _ -> Bool

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
    | Numeric (a, a_s), Numeric (b, b_s) ->
        let f_comp = Ast.compare_numeric_format a b in
        if f_comp = 0 then a_s - b_s else f_comp
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
    | Pointer a, Pointer b -> compare a b
    | Pointer _, _ -> 1
    | _, Pointer _ -> -1
    | Named (a_n, a_dt), Named (b_n, b_dt) ->
        let name_compare = String.compare a_n b_n in
        if name_compare = 0 then compare a_dt b_dt else name_compare

  let rec sexp_of_t a =
    match a with
    | Void -> Sexp.Atom "Void"
    | Bool -> Sexp.Atom "Bool"
    | Numeric (format, size) -> (
        let size = string_of_int size in
        match format with
        | Floating -> Sexp.Atom ("Floating" ^ size)
        | Signed -> Sexp.Atom ("Signed" ^ size)
        | Unsigned -> Sexp.Atom ("Unsigned" ^ size))
    | Tuple li -> List.sexp_of_t sexp_of_t li
    | Pointer a -> Sexp.List [ Sexp.Atom "Pointer"; sexp_of_t a ]
    | Named (name, alias) ->
        Sexp.List [ Sexp.Atom "Named"; Sexp.Atom name; sexp_of_t alias ]

  let hash a = sexp_of_t a |> Sexp.hash
end

let equal_data_type a b = DataType.compare a b |> ( = ) 0

let rec name_data_type a =
  match a with
  | Void -> "_void"
  | Bool -> "_bool"
  | Numeric (a, s) ->
      let s = string_of_int (s * 8) in
      (match a with Floating -> "_f" | Signed -> "_s" | Unsigned -> "_u") ^ s
  | Tuple f -> "_tuple" ^ String.concat ~sep:"_" (List.map ~f:name_data_type f)
  | Pointer i -> "_pointer" ^ name_data_type i
  | Named (n, _) -> n

let rec find_tuples dt =
  match dt with
  | Tuple f -> List.cons (Tuple f) (List.map ~f:find_tuples f |> List.concat)
  | Named (_, a) -> find_tuples a
  | _ -> []

type expression =
  | Literal of float * data_type * location_information
  | Variable of string * data_type * location_information
  | Call of string * expression list * data_type * location_information
  | Binary of expression * expression * Ast.binary_operation * location_information
  | TupleConstruction of string * expression list * location_information
  | TupleAccess of expression * int * data_type * location_information
  | IndexDeref of expression * int * location_information

let rec expr_type expr =
  match expr with
  | Literal (_, dt, _) -> dt
  | Variable (_, dt, _) -> dt
  | Call (_, _, dt, _) -> dt
  | Binary (lhs, _, op, _) -> op_return_type op (expr_type lhs)
  | TupleConstruction (_, factors, _) -> Tuple (List.map ~f:expr_type factors)
  | TupleAccess (_, index, dt, li) -> 
    begin
      match dt with 
      | Tuple factor_tys -> begin 
      match List.nth factor_tys index  with 
       | Some dt -> dt
       | None -> raise (Syntax_error (Printf.sprintf "tuple access out of bounds (%s)" (Ast.print_li li)))
      end
      | _ -> raise (Type_error (Printf.sprintf "can only access tuples (%s)" (Ast.print_li li)))
    end
  | IndexDeref (expr, _, li) -> let ty = expr_type expr in 
  begin
    match ty with 
      | Pointer ty -> ty 
      | _ -> raise (Type_error (Printf.sprintf "can only dereference pointers (%s)" (Ast.print_li li)))
  end

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
  used_types : data_type list;
}

let rec print_dt dt =
  match dt with
  | Void -> "Void"
  | Bool -> "Bool"
  | Numeric (a, s) -> Printf.sprintf "Numeric (%s, %d)" (Ast.print_nf a) s
  | Tuple factors ->
      let factors = String.concat ~sep:", " (List.map ~f:print_dt factors) in
      Printf.sprintf "Numeric (%s)" factors
  | Pointer i -> Printf.sprintf "Pointer (%s)" (print_dt i)
  | Named (name, dt) -> Printf.sprintf "Named (%s, %s)" name (print_dt dt)

let debug_print { externs; functions; aliases; used_types } =
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
    | Binary _ -> raise Unimplemented
    | TupleConstruction _ -> raise Unimplemented
    | TupleAccess _ -> raise Unimplemented
    | IndexDeref _ -> raise Unimplemented
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
  let used_types = List.map ~f:print_dt used_types |> String.concat ~sep:"\n" in
  externs ^ "\n" ^ functions ^ "\n" ^ aliases ^ "\n" ^ used_types
