open Core
open General

type data_type =
  | Void of location_information
  | Bool of location_information
  | Numeric of numeric_format * int option * location_information
  | Tuple of data_type list * location_information
  | Pointer of data_type * location_information
  | Named of string * location_information

and binary_operation =
  | Add of location_information
  | Sub of location_information
  | Mul of location_information
  | Equals of location_information
  | NotEquals of location_information
  | Less of location_information
  | Greater of location_information

type unary_operation = Not of location_information

and location =
  | IndexAccess of int * location_information
  | NamedAccess of string * location_information
  | DerefAccess of expression * location_information

and writable_expression = string * location list * location_information

and expression =
  | Literal of float * data_type option * location_information
  | Variable of writable_expression * location_information
  | Unary of expression * unary_operation * location_information
  | Binary of expression * expression * binary_operation * location_information
  | Call of string * expression list * location_information
  | TupleBuild of expression list * location_information
  | TupleAccess of expression * int * location_information

and statement =
  | Expression of expression * location_information
  | Assignment of string * data_type * expression * location_information
  | Reassignment of writable_expression * expression * location_information
  | If of expression * code_block * location_information
  | While of expression * code_block * location_information

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

type item =
  | Extern of Extern.t * location_information
  | Function of Function.t * location_information
  | Type of string * data_type * location_information

let parse_numeric_type s li =
  (*This function should never cause as long as the argument is a well
     formatted string with regard to the lexer *)
  let ty =
    match String.get s 0 with
    | 'f' -> Floating
    | 'i' -> Signed
    | 'u' -> Unsigned
    | _ -> raise (Invalid_argument "not well formatted numeric literal")
  in
  if String.is_substring_at s ~pos:1 ~substring:"size" then
    Numeric (ty, None, li)
  else
    let size = String.subo s ~pos:1 |> int_of_string |> ( / ) 8 in
    Numeric (ty, Some size, li)

let parse_numeric_literal s li =
  (* An exception should not be raised here because the split should always
     be able to happen. All strings passed into this function are assumed to
     have an '_' seperating the number and the type *)

  match String.lsplit2 s ~on:'_' with 
    | Some (value, ty) -> let first_col, last_col = li.columns in
    let first_col = first_col + String.length value in
    let sub_li =
      { file = li.file; lines = li.lines; columns = (first_col, last_col) }
    in
    let value = float_of_string value in
    Literal (value, Some (parse_numeric_type ty sub_li), li)
    | None -> Literal (float_of_string s, None, li)
  
