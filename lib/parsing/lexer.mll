{
  open Parser
  open Lexing

  let advance_line lexbuf =
    let position = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { position with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = position.pos_lnum + 1
      } 

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = '-'? digit+
let float_constant = '-'? digit+ '.' digit+
let identifier = alpha (alpha | digit | '_')*

let whitespace = [' ' '\t']+
let new_line = '\n' | "\r\n"

rule token = parse
  | "fun"    { FUN }
  | "struct" { STRUCT }
  | "enum"   { ENUM }
  | "let"    { LET }
  | "mut"    { MUT }
  | "{"      { LBRACK }
  | "}"      { RBRACK }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "["      { LBLOCK }
  | "]"      { RBLOCK }
  | "<"      { LANGLE }
  | ">"      { RANGLE }
  | ":"      { COLON }
  | ";"      { SCOLON }
  | ","      { COMMA }
  | "."      { PERIOD }
  | "="      { EQUALS }
  | "+"      { ADD }
  | "-"      { SUBTRACT }
  | "*"      { MULTIPLY }
  | "/"      { DIVIDE }
  | "->"     { ARROW }
  | "usize"  { USIZE }
  | "isize"  { ISIZE }
  | "fsize"  { FSIZE }
  | int_constant { INT_LITERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | float_constant { FLOAT_LITERAL (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | new_line { advance_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
