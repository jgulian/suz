exception Unimplemented
exception Syntax_error of string
exception Type_error of string

type location_information = {
  file : string;
  lines : int * int;
  columns : int * int;
}

let empty_location_information = { file = ""; lines = (0, 0); columns = (0, 0) }

let print_li li =
  let a, b = li.lines in
  let c, d = li.columns in
  Printf.sprintf "%s, %d-%d, %d-%d" li.file a b c d

type numeric_format = Floating | Signed | Unsigned

let numeric_format_id nf =
  match nf with Floating -> 0 | Signed -> 1 | Unsigned -> 2

let compare_numeric_format a b = numeric_format_id a - numeric_format_id b

let li (p : Lexing.position * Lexing.position) =
  let a, b = p in
  {
    file = a.pos_fname;
    lines = (a.pos_lnum, b.pos_lnum);
    columns = (a.pos_cnum - a.pos_bol + 1, b.pos_cnum - b.pos_bol + 1);
  }
