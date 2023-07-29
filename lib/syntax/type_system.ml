type numeric_format = Signed | Unsigned | Float

and data_type =
  | VoidType
  | String
  | NumericType of numeric_format * int option
  | NamedType of string
  | Tuple of data_type list

and struct_definition = {
  struct_name : string;
  fields : (string * data_type) list;
}

and enum_definition = {
  enum_name : string;
  variants : (string * data_type) list;
}

(* Unions are not able to be defined by users, but are used by the compiler
   to implement enums *)
and union_definition = {
  union_name : string;
  values : (string * data_type) list;
}

let rec type_equals a b =
  match (a, b) with
  | VoidType, VoidType -> true
  | String, String -> true
  | NumericType (f_a, s_a), NumericType (f_b, s_b) -> f_a == f_b && s_a == s_b
  | NamedType name_a, NamedType name_b -> name_a = name_b
  | Tuple tp_a, Tuple tp_b -> List.equal type_equals tp_a tp_b
  | _ -> false
