cmake_minimum_required(VERSION 3.22.2)
set(CMAKE_CXX_STANDARD 14)

project(suz)

# find_package(LLVM REQUIRED CONFIG)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(SuzParser parser.yy ${CMAKE_CURRENT_BINARY_DIR}/bison/parser.cpp
             DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/bison/parser.hpp)

FLEX_TARGET(SuzLexer scanner.ll ${CMAKE_CURRENT_BINARY_DIR}/flex/scanner.cpp
            DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/flex/scanner.hpp)
#ADD_FLEX_BISON_DEPENDENCY(SuzLexer SuzParser)

message(info ${BISON_SuzParser_OUTPUTS} ${FLEX_SuzLexer_OUTPUTS})

add_executable(suz main.cpp driver.cpp ${BISON_SuzParser_OUTPUTS} ${FLEX_SuzLexer_OUTPUTS})
message(${BISON_SuzParser_OUTPUT_HEADER})
target_include_directories(suz PRIVATE include 
    ${CMAKE_CURRENT_BINARY_DIR}/flex 
    ${CMAKE_CURRENT_BINARY_DIR}/bison)