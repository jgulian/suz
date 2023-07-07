#pragma once

#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

using void_type = std::monostate;
using named_type = std::string;

enum struct numeric_type {
  numeric_type_u32,
  numeric_type_i32,
  numeric_type_f32,
};

using type = std::variant<void_type, named_type, numeric_type>;

struct struct_definition {
  struct_definition(std::string name,
                    std::vector<std::pair<std::string, type>> fields);

  std::string name;
  std::vector<std::pair<std::string, type>> fields;
};

struct enum_definition {
  enum_definition(std::string name,
                  std::vector<std::pair<std::string, type>> variants);

  std::string name;
  std::vector<std::pair<std::string, type>> variants;
};

using literal_expression = std::variant<std::string, int64_t, float>;
using variable_expression = std::string;

struct binary_operation_expression;
struct call_expression;
struct code_block;
using expression = std::variant<literal_expression,
                                variable_expression,
                                std::shared_ptr<binary_operation_expression>,
                                std::shared_ptr<call_expression>,
                                std::shared_ptr<code_block>>;

enum struct binary_operation {
  binary_operation_add,
  binary_operation_sub,
  binary_operation_mul,
  binary_operation_div
};

struct binary_operation_expression {
  binary_operation_expression(binary_operation operation,
                              expression left_hand_side,
                              expression right_hand_side);

  binary_operation operation;
  expression left_hand_side, right_hand_side;
};

struct call_expression {
  call_expression(std::string function_name, std::vector<expression> arguments);

  std::string function_name;
  std::vector<expression> arguments;
};

type expression_type(expression expr);

struct assignment {
  assignment(std::string variable,
             std::optional<type> variable_type,
             bool mut,
             bool reassignment,
             expression value);

  std::string variable;
  std::optional<type> variable_type;
  bool mut, reassignment;
  expression value;
};

using statement = std::variant<expression, assignment>;

struct code_block {
  code_block(std::vector<statement> statements,
             std::optional<expression> value);

  std::vector<statement> statements;
  std::optional<expression> value;
};

struct function_definition {
  function_definition(std::string name,
                      std::vector<std::pair<std::string, type>> arguments,
                      type return_type,
                      code_block block);

  std::string name;
  std::vector<std::pair<std::string, type>> arguments;
  type return_type;
  code_block block;
};

using type_definition = std::variant<struct_definition, enum_definition>;

std::string type_name(type_definition &t);
void type_dependents(type_definition &t, std::vector<std::string> &v);

struct module_data {
  std::vector<type_definition> type_definitions;
  std::vector<function_definition> function_definitions;
};