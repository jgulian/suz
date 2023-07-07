#include "syntax.hpp"

struct_definition::struct_definition(
    std::string name, std::vector<std::pair<std::string, type>> fields)
    : name(name), fields(fields)
{
}

enum_definition::enum_definition(
    std::string name, std::vector<std::pair<std::string, type>> variants)
    : name(name), variants(variants)
{
}

binary_operation_expression::binary_operation_expression(
    binary_operation operation,
    expression left_hand_side,
    expression right_hand_side)
    : operation(operation), left_hand_side(left_hand_side),
      right_hand_side(right_hand_side)
{
}

call_expression::call_expression(std::string function_name,
                                 std::vector<expression> arguments)
    : function_name(function_name), arguments(arguments)
{
}

type expression_type(expression expr) {}

assignment::assignment(std::string variable,
                       std::optional<type> variable_type,
                       bool mut,
                       bool reassignment,
                       expression value)
    : variable(variable), variable_type(variable_type), mut(mut),
      reassignment(reassignment), value(std::move(value))
{
}

code_block::code_block(std::vector<statement> statements,
                       std::optional<expression> value)
    : statements(statements), value(value)
{
}

function_definition::function_definition(
    std::string name,
    std::vector<std::pair<std::string, type>> arguments,
    type return_type,
    code_block block)
    : name(name), arguments(arguments), return_type(return_type), block(block)
{
}

std::string type_name(type_definition &t)
{
  if (struct_definition *s = std::get_if<struct_definition>(&t)) {
    return s->name;
  } else if (enum_definition *e = std::get_if<enum_definition>(&t)) {
    return e->name;
  } else {
    throw "unknwon type definition";
  }
}

void type_dependents(type_definition &t, std::vector<std::string> &v)
{
  if (struct_definition *s = std::get_if<struct_definition>(&t)) {
    for (auto &field : s->fields) {
      if (auto *name = std::get_if<named_type>(&field.second)) {
        v.push_back(*name);
      }
    }
  } else if (enum_definition *e = std::get_if<enum_definition>(&t)) {
    for (auto &variant : e->variants) {
      if (auto *name = std::get_if<named_type>(&variant.second)) {
        v.push_back(*name);
      }
    }
  } else {
    throw "unknwon type definition";
  }
}