#pragma once

#include <llvm/IR/IRBuilder.h>

#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "algorithm.hpp"
#include "syntax.hpp"

class codegen
{
public:
  codegen(std::string module_name, module_data module);

  void perform();

private:
  void type_codegen();
  void function_codegen();
  llvm::Value *expression_codegen(
      std::unordered_map<std::string, std::pair<llvm::AllocaInst *, bool>>
          variables,
      expression &expr);

  llvm::Type *generate_type(type &t);

  std::string module_name;
  module_data module;
  llvm::LLVMContext context;
  llvm::IRBuilder<> ir_builder;
  llvm::Module current_module;
};