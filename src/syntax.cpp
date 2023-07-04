#include "syntax.hpp"

struct_type::struct_type(std::string name,
                         std::vector<std::pair<std::string, type>> fields)
    : name(name), fields(fields)
{
}

enum_type::enum_type(std::string name,
                     std::vector<std::pair<std::string, type>> variants)
    : name(name), variants(variants)
{
}

variable_expression::variable_expression(std::string name) : name(name) {}

binary_operation_expression::binary_operation_expression(
    binary_operation operation, expression left_hand_side,
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

inner_expression::inner_expression(expression inner) : inner(inner) {}

assignment::assignment(std::string variable, std::optional<type> variable_type,
                       bool mut, bool reassignment, expression value)
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
    std::string name, std::vector<std::pair<std::string, type>> arguments,
    type return_type, code_block block)
    : name(name), arguments(arguments), return_type(return_type), block(block)
{
}