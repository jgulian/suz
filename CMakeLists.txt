cmake_minimum_required(VERSION 3.22.2)
set(CMAKE_CXX_STANDARD 17)

project(Ex)

find_package(LLVM REQUIRED CONFIG)

llvm_map_components_to_libnames(llvm_libs support core irreader)

find_package(BISON)
find_package(FLEX)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/generated/)

BISON_TARGET(SuzParser src/parser.yy ${CMAKE_CURRENT_BINARY_DIR}/generated/generated/parser.cpp
DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/generated/generated/parser.hpp)

FLEX_TARGET(SuzLexer src/scanner.ll ${CMAKE_CURRENT_BINARY_DIR}/generated/generated/scanner.cpp
DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/generated/generated/scanner.hpp)

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_executable(suz 
    src/main.cpp 

    src/algorithm.cpp
    src/codegen.cpp
    src/driver.cpp
    src/syntax.cpp

    ${BISON_SuzParser_OUTPUTS} 
    ${FLEX_SuzLexer_OUTPUTS}
    )

target_include_directories(suz PRIVATE src/include ${CMAKE_CURRENT_BINARY_DIR}/generated)
target_link_libraries(suz PRIVATE ${llvm_libs})