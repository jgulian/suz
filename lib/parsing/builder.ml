open Lexing

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse_channel c = 
  let lexbuf = Lexing.from_channel c in
  try
    Parser.items Lexer.token lexbuf
  with Parser.Error ->
    raise (Failure ("Syntax Error at " ^ (pos_string lexbuf.lex_curr_p)))