open Core
open General

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
        let f_comp = compare_numeric_format a b in
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

type unary_operation = Not

type expression =
  | Literal of float * data_type * location_information
  | Variable of string * data_type * location_information
  | Call of string * expression list * data_type * location_information
  | Unary of expression * unary_operation * data_type * location_information
  | Binary of
      expression
      * expression
      * Ast.binary_operation
      * data_type
      * location_information
  | TupleConstruction of expression list * data_type * location_information
  | TupleAccess of expression * int * data_type * location_information
  | IndexDeref of expression * expression * data_type * location_information

type location =
  | Access of int * location_information
  | Deref of expression * location_information

let expr_type expr =
  match expr with
  | Literal (_, dt, _) -> dt
  | Variable (_, dt, _) -> dt
  | Call (_, _, dt, _) -> dt
  | Unary (_, _, dt, _) -> dt
  | Binary (_, _, _, dt, _) -> dt
  | TupleConstruction (_, dt, _) -> dt
  | TupleAccess (_, _, dt, _) -> dt
  | IndexDeref (_, _, dt, _) -> dt

type statement =
  | Expression of expression * location_information
  | Assignment of
      string
      * location list
      * data_type
      * expression
      * bool
      * location_information
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
    parameters : (string * bool * data_type) list;
    body : code_block;
    location : location_information;
  }
end

type build_module = {
  externs : Extern.t list;
  functions : Function.t list;
  aliases : (string * data_type) list;
}
