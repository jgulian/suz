#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "codegen.hpp"
#include "driver.hpp"

int main(int argc, char **argv)
{
  std::cout << "Hello World" << std::endl;

  std::ifstream file{"../test.suz"};
  std::stringstream buffer;
  buffer << file.rdbuf();
  file.close();
  std::string file_text = buffer.str();

  driver parsing_driver;
  parsing_driver.trace_scanning = false;
  parsing_driver.trace_parsing = false;
  parsing_driver.parse(file_text, "test.suz");

  codegen generator("test", std::move(parsing_driver.result));
}