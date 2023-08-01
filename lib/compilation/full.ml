let create_build_dir dir =
  let _ = Sys.command ("mkdir -p " ^ dir) in
  ()

let compile file c_files out =
  let c_files = String.concat " " c_files in
  print_endline (Printf.sprintf "clang -o %s %s %s" out file c_files);
  let _ = Sys.command (Printf.sprintf "clang -o %s %s %s" out file c_files) in
  ()
