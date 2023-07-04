#pragma once

#include <llvm/IR/IRBuilder.h>

#include <memory>

#include "syntax.hpp"

class codegen
{
public:
  codegen(std::string module_name, module_data module);

  void perform();

  void function_codegen(function_definition &definition);
  llvm::FunctionType *function_type(function_definition &definition);

  void struct_codegen(struct_type &definition);
  void enum_codegen(enum_type &definition);

private:
  std::string module_name;
  module_data module;
  llvm::LLVMContext context;
  llvm::IRBuilder<> ir_builder;
  llvm::Module current_module;
};