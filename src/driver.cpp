#include "driver.hpp"

int driver::parse(const std::string &text, const std::string &location_name)
{
  scan_begin(text);
  location.initialize(&location_name);
  yy::parser parser(*this);
  parser.set_debug_level(trace_parsing);
  int res = parser.parse();
  return res;
}