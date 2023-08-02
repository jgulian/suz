{
  open Parser
  open Lexing
  open Structure

  let advance_line lexbuf =
    let position = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { position with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = position.pos_lnum + 1
      } 

  let li lexbuf = 
    Ast.li (lexbuf.lex_start_p, lexbuf.lex_curr_p)

}

let non_zero = ['1'-'9']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let numeric_type = ('f' | 'u' | 'i') ("size")
(* '8' | "16" | "32" | "64" |  *)

let index = non_zero digit*
let int_constant = '-'? digit+ '_' numeric_type
let float_constant = '-'? digit+ '.' digit+ '_' numeric_type
let identifier = alpha (alpha | digit | '_')*

let whitespace = [' ' '\t']+
let new_line = '\n' | "\r\n"

rule token = parse
  | "fun"    { FUN }
  | "ext"    { EXT }
  | "let"    { LET }
  | "if"     { IF }
  | "while"  { WHILE }
  | "type"   { TYPE }
  | "{"      { LBRACK }
  | "}"      { RBRACK }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | ":"      { COLON }
  | ";"      { SCOLON }
  | "."      { PERIOD }
  | ","      { COMMA }
  | "="      { EQUALS }
  | "=="     { DEQUALS }
  | "!="     { NEQUALS }
  | "<"      { LANGLE }
  | ">"      { RANGLE }
  | "+"      { ADD }
  | "-"      { SUB }
  | "*"      { MUL }
  | "->"     { ARROW }
  | ":="     { ASSIGN }
  | int_constant { NUM_LITERAL (Ast.parse_numeric_literal (Lexing.lexeme lexbuf) (li lexbuf)) }
  | float_constant { NUM_LITERAL (Ast.parse_numeric_literal (Lexing.lexeme lexbuf) (li lexbuf)) }
  | numeric_type { NUMERIC_TYPE (Ast.parse_numeric_type (Lexing.lexeme lexbuf) (li lexbuf)) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | index { INDEX (int_of_string (Lexing.lexeme lexbuf))  }
  | whitespace { token lexbuf }
  | new_line { advance_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
