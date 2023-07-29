open Parsing
open Compilation

let file = "test.suz"

let () =
  let input_channel = open_in file in
  try
    let items = Builder.parse_channel input_channel in
    let items = Verification.verify_typing items in
    let ctx = Translation.create_context () in
    Translation.codegen_module items ctx
  with e ->
    close_in_noerr input_channel;
    let str = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s%s\n" str stack;
    raise e
