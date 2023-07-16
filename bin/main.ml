open Parsing

let file = "test.suz"

let () =
  let input_channel = open_in file in
  try
    let items = Builder.parse_channel input_channel in
    print_int (List.length items)

  with e ->
    close_in_noerr input_channel;
    let str = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s%s\n" str stack;
    raise e
