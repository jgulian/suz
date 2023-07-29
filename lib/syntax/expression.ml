open Type_system

type literal_expression = Number of numeric_format * float | Str of string
and binary_operation = Add | Sub | Mul | Div | Less | Greater | Access

and expression =
  | LiteralExpression of literal_expression
  | VariableExpression of string
  | BinaryExpression of  { op : binary_operation; lhs : expression;rhs : expression;}
  | CallExpression of string * expression list
  (*| AccessExpression of expression * string *)
  | StructConstructionExpression of string * (string * expression) list

and statement = Expression of expression | Assignment of string * data_type * expression
and code_block = statement list * expression

and function_definition = {
  name : string;
  parameters : (string * data_type) list;
  return_type : data_type;
  block : code_block;
}

and item =
  | FunctionDefinition of function_definition
  | StructDefinition of struct_definition
  | EnumDefinition of enum_definition
  | UnionDefinition of union_definition

let name_of_item (i : item) : string =
  match i with
  | FunctionDefinition def -> def.name
  | StructDefinition def -> def.struct_name
  | EnumDefinition def -> def.enum_name
  | UnionDefinition def -> def.union_name
