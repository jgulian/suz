open Type_system

type literal_expression = Number of numeric_type * float | Str of string
and variable_expression = string
and binary_operation = Add | Sub | Mul | Div | Less | Greater | Access

and binary_expression = {
  op : binary_operation;
  lhs : expression;
  rhs : expression;
}

and call_expression = string * expression list
and access_expression = string

and struct_construction_expression = string * (string * expression) list

and expression =
  | LiteralExpression of literal_expression
  | VariableExpression of variable_expression
  | BinaryExpression of binary_expression
  | CallExpression of call_expression
  | AccessExpression of access_expression
  | StructConstructionExpression of struct_construction_expression

and assignment = string * expression * bool
and statement = Expression of expression | Assignment of assignment
and code_block = statement list * expression

and function_definition = {
  name : string;
  function_generics : string list;
  parameters : (string * data_type) list;
  return_type : data_type;
  block : code_block;
}

and item =
  | FunctionDefinition of function_definition
  | StructDefinition of struct_definition
  | EnumDefinition of enum_definition;;


let name_of_item (i: item) : string = 
  match i with 
    | FunctionDefinition def -> def.name
    | StructDefinition def -> def.struct_name
    | EnumDefinition def -> def.enum_name