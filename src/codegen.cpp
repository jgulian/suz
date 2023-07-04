#include "codegen.hpp"

codegen::codegen(std::string module_name, module_data module)
    : module_name(module_name), module(std::move(module)), context(),
      ir_builder(context), current_module(module_name, context)
{
}

void codegen::perform() {}

void codegen::function_codegen(function_definition &definition)
{
  llvm::FunctionType *functionType = function_type(definition);
}

llvm::FunctionType *codegen::function_type(function_definition &definition) {}