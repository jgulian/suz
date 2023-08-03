open Core
open Llvm
open Structure
open Tsr

exception Unexpected_type of string
exception Internal_issue of string
exception Unsupported_type of string

let rec get_data_type context dt =
  let get_data_type_c = get_data_type context in
  match dt with
  | Void -> void_type context
  | Bool -> i1_type context
  | Numeric (format, size) -> (
      match format with
      | Floating ->
          if size = 4 then float_type context
          else if size = 8 then double_type context
          else raise (Unsupported_type "float only supports f32 and f64")
      | Signed | Unsigned -> (
          match size with
          | 1 -> i8_type context
          | 2 -> i16_type context
          | 4 -> i32_type context
          | 8 -> i64_type context
          | _ ->
              raise
                (Unsupported_type "integers only supports 8, 16, 32, and 64")))
  | Tuple factors ->
      struct_type context
        (Array.of_list (List.map ~f:(fun x -> get_data_type_c x) factors))
  | Pointer inner -> pointer_type (get_data_type_c inner)
  | Named (name, _) -> named_struct_type context name
