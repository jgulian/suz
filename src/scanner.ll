%{ /* -*- C++ -*- */
#include <cerrno>
#include <climits>
#include <cstdlib>
#include <string>
#include "driver.hpp"

#undef yywrap
#define yywrap() 1

#if defined __GNUC__ && 7 <= __GNUC__
# pragma GCC diagnostic ignored "-Wnull-dereference"
#endif
%}

%option noyywrap nounput batch debug noinput

id [a-zA-Z][a-zA-Z_0-9]*
int   [0-9]+
blank [ \t]

%{
  # define YY_USER_ACTION  loc.columns (yyleng);
%}

%%

%{
  yy::location& loc = drv.location;
  loc.step ();
%}
{blank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();

"-"      return yy::parser::make_MINUS   (loc);
"+"      return yy::parser::make_PLUS    (loc);
"*"      return yy::parser::make_STAR    (loc);
"/"      return yy::parser::make_SLASH   (loc);
"("      return yy::parser::make_LPAREN  (loc);
")"      return yy::parser::make_RPAREN  (loc);
"="      return yy::parser::make_ASSIGN  (loc);
"{"      return yy::parser::make_LBRACK  (loc);
"}"      return yy::parser::make_RBRACK  (loc);
";"      return yy::parser::make_SCOLON  (loc);
":"      return yy::parser::make_COLON   (loc);
","      return yy::parser::make_COMMA   (loc);
"let"    return yy::parser::make_LET     (loc);
"fn"     return yy::parser::make_FN      (loc);
"u32"    return yy::parser::make_NTU32   (loc);
"i32"    return yy::parser::make_NTI32   (loc);
"f32"    return yy::parser::make_NTF32   (loc);
"->"     return yy::parser::make_RETURNS (loc);
"<>"     return yy::parser::make_REENTRY (loc);
"struct" return yy::parser::make_STRUCT  (loc);
"enum"   return yy::parser::make_ENUM    (loc);
"()"     return yy::parser::make_VOID    (loc);
"mut"    return yy::parser::make_MUT     (loc);

{int}      {
  errno = 0;
  long n = strtol (yytext, NULL, 10);
  if (! (INT_MIN <= n && n <= INT_MAX && errno != ERANGE))
    throw yy::parser::syntax_error (loc, "integer is out of range: "
                                    + std::string(yytext));
  return yy::parser::make_NUMBER (n, loc);
}
{id}       return yy::parser::make_IDENTIFIER (yytext, loc);
.          {
             throw yy::parser::syntax_error
               (loc, "invalid character: " + std::string(yytext));
}
<<EOF>>    return yy::parser::make_END (loc);

%%

void
driver::scan_begin(const std::string &text)
{
  yy_flex_debug = trace_scanning;
  yy_scan_string(text.c_str());
}