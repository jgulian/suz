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

let ( -- ) i j =
  let rec aux n r = if n < i then r else aux (n - 1) (n :: r) in
  aux (j - 1) []

let codegen_tuple context current_mod tuple_dt =
  let get_data_type_c = get_data_type context in
  let tuple =
    match tuple_dt with
    | Tsr.Tuple f -> f
    | _ -> raise (Internal_issue "expected tuple")
  in
  let hash = DataType.hash tuple_dt |> string_of_int in
  let factor_lltys =
    List.map ~f:(fun x -> get_data_type_c x) tuple |> Array.of_list
  in
  let ty = struct_type context factor_lltys in
  let func_ty = function_type ty factor_lltys in
  let name = "_tuple_build_" ^ hash in
  let func = define_function name func_ty current_mod in
  let builder = entry_block func |> builder_at_end context in
  let location = build_alloca ty "" builder in
  let store_element index =
    let pointer = build_struct_gep location index "" builder in
    let _ = build_store (param func index) pointer builder in
    ()
  in
  0 -- List.length tuple |> List.iter ~f:store_element;
  let _ = (build_load location "" builder |> build_ret) builder in
  let codegen_tuple_access index =
    let name = Printf.sprintf "_tuple_access_%s_%d" hash index in
    let factor_ty = Array.get factor_lltys index in
    let func_ty = function_type factor_ty (Array.of_list [ ty ]) in
    let func = define_function name func_ty current_mod in
    let builder = entry_block func |> builder_at_end context in
    let arg = param func 0 in
    let location = build_alloca ty "" builder in
    let _ = build_store arg location builder in
    let pointer = build_struct_gep location index "" builder in
    let value = build_load pointer "" builder in
    let _ = build_ret value builder in
    ()
  in
  0 -- List.length tuple |> List.iter ~f:codegen_tuple_access

let codegen_pointer context current_mod pointer_dt =
  let get_data_type_c = get_data_type context in
  let inner_dt =
    match pointer_dt with
    | Pointer inner -> inner
    | _ -> raise (Internal_issue "expected pointer")
  in
  let pointer_ty = get_data_type_c pointer_dt in
  let inner_ty = get_data_type_c inner_dt in
  let func_ty = function_type inner_ty (Array.of_list [ pointer_ty ]) in
  let name = "_deref" ^ (DataType.hash pointer_dt |> string_of_int) in
  let func = define_function name func_ty current_mod in
  let builder = entry_block func |> builder_at_end context in
  let _ = ((param func 0 |> build_load) "" builder |> build_ret) builder in
  ()

let codegen_used_type context current_mod dt =
  match dt with
  | Pointer _ -> codegen_pointer context current_mod dt
  | Tuple _ -> codegen_tuple context current_mod dt
  | _ -> ()
