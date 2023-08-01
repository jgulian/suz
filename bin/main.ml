open Parsing
open Structure
open Compilation

let file = "test.suz"
let rec newlines n = if n = 0 then () else print_newline (newlines (n - 1))

let () =
  let input_channel = open_in file in
  try
    let items = Builder.parse_channel input_channel (Some file) in
    print_endline (Ast.debug_print items);
    newlines 5;
    let main_mod = Lowering.lower items in
    print_endline (Tsr.debug_print main_mod);
    newlines 5;
    let llctx = Llvm.global_context () in
    let mod_name = "main" in
    let main_mod = Code_generation.generate_code mod_name main_mod llctx in
    Llvm.dump_module main_mod;
    Full.create_build_dir ".build";
    Llvm.print_module ".build/test.ll" main_mod;
    Full.compile ".build/test.ll" ["runtime/io.c"] ".build/main"
  with e ->
    close_in_noerr input_channel;
    let str = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s%s\n" str stack;
    raise e
