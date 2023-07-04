#pragma once

#include <string>
#include <vector>

#include "generated/parser.hpp"
#include "syntax.hpp"

#define YY_DECL yy::parser::symbol_type yylex(driver &drv)
// ... and declare it for the parser's sake.
YY_DECL;

struct driver
{
  module_data result;
  std::string file;
  bool trace_parsing = false;
  bool trace_scanning = false;
  yy::location location;

  int parse(const std::string &text, const std::string &location_name);

  void scan_begin(const std::string &text);
};