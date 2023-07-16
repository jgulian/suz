type numeric_format = Signed | Unsigned | Float
and numeric_type = { format : numeric_format; byte_count : int option }
and named_type = string
and tuple = data_type list

and data_type =
  | VoidType
  | NumericType of numeric_type
  | NamedType of named_type
  | Tuple of tuple

and struct_definition = {
  struct_name : string;
  struct_generics : string list;
  fields : (string * data_type) list;
}

and enum_definition = {
  enum_name : string;
  enum_generics : string list;
  variants : (string * data_type) list;
}
