#include "codegen.hpp"

codegen::codegen(std::string module_name, module_data module)
    : module_name(module_name), module(std::move(module)), context(),
      ir_builder(context), current_module(module_name, context)
{
}

void codegen::perform()
{
  type_codegen();
  function_codegen();
  current_module.print(llvm::errs(), nullptr, false, true);
}

void codegen::type_codegen()
{
  std::unordered_map<std::string, size_t> name_to_id;
  std::vector<std::vector<size_t>> dependencies;

  for (auto &type_definition : module.type_definitions) {
    auto name = type_name(type_definition);
    if (name_to_id.find(name) != name_to_id.end())
      throw "duplicate name error";
    name_to_id[name] = name_to_id.size();
    dependencies.emplace_back();
  }

  for (auto &type_definition : module.type_definitions) {
    auto name = type_name(type_definition);
    auto id = name_to_id[name];
    std::vector<std::string> v;

    type_dependents(type_definition, v);
    for (auto &field : v) {
      dependencies[id].push_back(name_to_id[field]);
    }
  }

  auto order = topological_sort(dependencies);

  for (auto i = order.rbegin(); i != order.rend(); ++i) {
    auto type_definition = module.type_definitions[*i];
    auto name = type_name(type_definition);
    llvm::StructType *new_type = llvm::StructType::create(context, name);

    if (struct_definition *definition =
            std::get_if<struct_definition>(&type_definition)) {
      std::vector<llvm::Type *> fields;
      for (auto &field : definition->fields) {
        fields.push_back(generate_type(field.second));
      }
      new_type->setBody(fields);
    } else if (enum_definition *definition =
                   std::get_if<enum_definition>(&type_definition)) {
      // I am unsure how to do this, but look at llvm::DataLayout
    } else {
      throw "unknwon type definition";
    }
  }
}

void codegen::function_codegen()
{
  for (auto &function_definition : module.function_definitions) {
    std::vector<llvm::Type *> parameter_types;
    for (auto &parameter : function_definition.arguments) {
      parameter_types.push_back(generate_type(parameter.second));
    }

    llvm::Type *return_type = generate_type(function_definition.return_type);
    llvm::FunctionType *function_type =
        llvm::FunctionType::get(return_type, parameter_types, false);
    llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                           function_definition.name, current_module);
  }

  for (auto &function_definition : module.function_definitions) {
    llvm::Function *function =
        current_module.getFunction(function_definition.name);
    llvm::BasicBlock *basic_block =
        llvm::BasicBlock::Create(context, function_definition.name, function);

    ir_builder.SetInsertPoint(basic_block);

    std::unordered_map<std::string, std::pair<llvm::AllocaInst *, bool>>
        variables;

    for (auto i = 0; i < function_definition.arguments.size(); i++) {
      auto &argument = function_definition.arguments[i];
      llvm::Type *parameter_type = function->getFunctionType()->getParamType(i);
      variables[argument.first] = {
          ir_builder.CreateAlloca(generate_type(argument.second), nullptr,
                                  llvm::Twine(argument.first)),
          false};
      ir_builder.CreateStore(function->getArg(i),
                             variables[argument.first].first);
    }

    for (auto statement : function_definition.block.statements) {
      if (assignment *a = std::get_if<assignment>(&statement)) {
        if (a->reassignment) {
          if (!variables[a->variable].second) {
            throw "variable is not mutable";
          }
        } else {
          variables[a->variable] = {
              ir_builder.CreateAlloca(generate_type(a->variable_type.value()),
                                      nullptr, llvm::Twine(a->variable)),
              a->mut};
        }
        ir_builder.CreateStore()
      } else if (expression *e = std::get_if<expression>(&statement)) {
        expression_codegen(variables, e);
      } else {
        throw "thing bad";
      }
    }

    if (function_definition.block.value.has_value()) {

    } else {
      ir_builder.CreateRetVoid();
    }
  }
}

llvm::Value *codegen::expression_codegen(
    std::unordered_map<std::string, std::pair<llvm::AllocaInst *, bool>>
        variables,
    expression &expr)
{
  if (literal_expression *literal = std::get_if<literal_expression>(&expr)) {
    if (std::string *string = std::get_if<std::string>(literal)) {
      throw "string literal expressions are not supported";
    } else if (int64_t *value = std::get_if<int64_t>(literal)) {
      return llvm::ConstantInt::get(context, *value);
    } else if (float *value = std::get_if<float>(literal)) {
      return llvm::ConstantFP::get(context, *value);
    }
  } else if (variable_expression *literal =
                 std::get_if<variable_expression>(&expr)) {
    return ir_builder.CreateLoad(variables[*literal].first);
  } else if (std::shared_ptr<binary_operation_expression> *binary_expression =
                 std::get_if<std::shared_ptr<binary_operation_expression>>(
                     &expr)) {
  } else if (std::shared_ptr<call_expression> *binary_expression =
                 std::get_if<std::shared_ptr<call_expression>>(&expr)) {

  } else if (std::shared_ptr<code_block> *block =
                 std::get_if<std::shared_ptr<code_block>>(&expr)) {
  } else {
    throw "unknown expression";
  }
}

llvm::Type *codegen::generate_type(type &t)
{
  if (std::get_if<void_type>(&t)) {
    return llvm::Type::getVoidTy(context);
  } else if (std::string *v = std::get_if<named_type>(&t)) {
    return llvm::StructType::getTypeByName(context, *v)->getPointerTo();
  } else if (numeric_type *v = std::get_if<numeric_type>(&t)) {
    switch (*v) {
    case numeric_type::numeric_type_u32:
      return llvm::Type::getInt32Ty(context);
    case numeric_type::numeric_type_i32:
      return llvm::Type::getInt32Ty(context);
    case numeric_type::numeric_type_f32:
      return llvm::Type::getFloatTy(context);
    default:
      throw "unknown type";
    }
  } else {
    throw "unknown type";
  }
}