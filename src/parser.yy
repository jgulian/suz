%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.1"
%defines

%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires {
  #include <string>
  #include <memory>
  #include <utility>

  #include "syntax.hpp"

  struct driver;
}

// The parsing context.
%param { driver& parsing_driver }

%locations

%define parse.trace
%define parse.error verbose

%code {
  #include "driver.hpp"
}

%define api.token.prefix {TOK_}
%token
  END  0    "end of file"
  ASSIGN    "="
  MINUS     "-"
  PLUS      "+"
  STAR      "*"
  SLASH     "/"
  LPAREN    "("
  RPAREN    ")"
  LBRACK    "{"
  RBRACK    "}"
  COLON     ":"
  SCOLON    ";"
  COMMA     ","
  LET       "let"
  FN        "fn"
  NTU32     "u32"
  NTI32     "i32"
  NTF32     "f32"
  RETURNS   "->"
  REENTRY   "<>"
  STRUCT    "struct"
  ENUM      "enum"
  VOID      "()"
  MUT       "mut"
;

%token <std::string> IDENTIFIER "identifier"
%token <int> NUMBER "number"

%type <std::vector<std::pair<std::string, type>>> parameter_list
%type <std::pair<std::string, type>> parameter

%type <std::vector<std::pair<std::string, type>>> variant_list
%type <std::pair<std::string, type>> variant

%type <type> return_declaration
%type <std::optional<code_block>> block

%type <std::vector<statement>> statements 
%type <statement> statement 
%type <std::optional<assignment>> assignment

%type <type> type
%type <expression> expression_statement
%type <expression> expression
%type <std::vector<expression>> expression_list

%printer { /* yyoutput << $$; */ } <*>;

%%

%start items;
items:
  %empty
| items item;

item:
  function
| struct_definition
| enum_definition;

struct_definition:
  "struct" "identifier" "{" parameter_list "}" { 
    parsing_driver.result.type_definitions.emplace_back(YY_MOVE(struct_definition($2, YY_MOVE($4)))); 
  };

enum_definition:
  "enum" "identifier" "{" variant_list "}" { 
    parsing_driver.result.type_definitions.emplace_back(YY_MOVE(enum_definition($2, YY_MOVE($4)))); 
  };

variant_list:
  %empty { $$; }
| variant { $$; }
| variant "," variant_list { std::swap($$, $3); $$.emplace_back(YY_MOVE($1)); };

variant:
  "identifier" "(" type ")" { $$ = {YY_MOVE($1), YY_MOVE($3)}; }

function:
  "fn" "identifier" "(" parameter_list ")" return_declaration block { 
    parsing_driver.result.function_definitions.emplace_back($2, YY_MOVE($4), YY_MOVE($6), YY_MOVE($7.value())); 
  };
| "fn" "identifier" "()" return_declaration block { 
    parsing_driver.result.function_definitions.emplace_back(
      $2, 
      std::vector<std::pair<std::string, type>>{}, 
      YY_MOVE($4), 
      YY_MOVE($5.value())
      ); 
  };

parameter_list:
  %empty                       { $$; }
| parameter                    { $$; $$.emplace_back(YY_MOVE($1)); }
| parameter "," parameter_list { std::swap($$, $3); $$.emplace_back(YY_MOVE($1)); };

parameter:
  "identifier" ":" type { $$ = {$1, YY_MOVE($3)}; };

return_declaration:
  %empty { $$ = void_type(); }
| "->" type { std::swap($$, $2); };

type:
  "()"         { $$ = void_type(); }
| "u32"        { $$ = numeric_type(numeric_type::numeric_type_u32); }
| "i32"        { $$ = numeric_type(numeric_type::numeric_type_u32); }
| "f32"        { $$ = numeric_type(numeric_type::numeric_type_u32); }
| "identifier" { $$ = named_type($1); };

block:
  "{" statements "}" { $$ = code_block(YY_MOVE($2), std::nullopt); }
| "{" statements expression "}" { $$ = code_block(YY_MOVE($2), YY_MOVE($3)); }

statements:
  %empty               { $$; }
| statements statement { std::swap($$, $1); $$.emplace_back(YY_MOVE($2)); };

statement:
  assignment           { $$ = YY_MOVE($1.value()); }
| expression_statement { $$ = YY_MOVE($1); };

assignment:
  "let" "identifier" ":" type "=" expression ";" { $$ = assignment($2, YY_MOVE($4), false, false, YY_MOVE($6)); };
| "let" "mut" "identifier" ":" type "=" expression ";" { $$ = assignment($3, YY_MOVE($5), true, false, YY_MOVE($7)); };
| "identifier" "=" expression ";" { $$ = assignment($1, std::nullopt, false, true, YY_MOVE($3)); };

expression_statement:
  expression ";" { $$ = YY_MOVE($1); };

expression_list:
  expression                     { $$; $$.emplace_back(YY_MOVE($1)); }
| expression_list "," expression { std::swap($$, $1); $$.emplace_back(YY_MOVE($3)); };

expression:
  expression "+" expression            { $$ = std::make_shared<binary_operation_expression>(binary_operation::binary_operation_add, YY_MOVE($1), YY_MOVE($3)); }
| expression "-" expression            { $$ = std::make_shared<binary_operation_expression>(binary_operation::binary_operation_sub, YY_MOVE($1), YY_MOVE($3)); }
| expression "*" expression            { $$ = std::make_shared<binary_operation_expression>(binary_operation::binary_operation_mul, YY_MOVE($1), YY_MOVE($3)); }
| expression "/" expression            { $$ = std::make_shared<binary_operation_expression>(binary_operation::binary_operation_div, YY_MOVE($1), YY_MOVE($3)); }
| "(" expression ")"                   { $$ = std::make_shared<inner_expression>(YY_MOVE($2)); }
| "identifier" "(" expression_list ")" { $$ = std::make_shared<call_expression>(YY_MOVE($1), YY_MOVE($3)); }
| "identifier" "()"                    { $$ = std::make_shared<call_expression>(YY_MOVE($1), std::vector<expression>()); }
| block                                { $$ = std::make_shared<code_block>(YY_MOVE($1.value())); }
| "identifier"                         { $$ = std::make_shared<variable_expression>($1); }
| "number"                             { $$ = literal_expression(static_cast<uint64_t>($1)); };

// might need to be above but should read the docs
%left "+" "-";
%left "*" "/";

%%

void
yy::parser::error (const location_type& l, const std::string& m)
{
  std::cerr << l << ": " << m << '\n';
}

