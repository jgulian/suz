open Syntax.Type_system
open Syntax.Expression

open Core

type name_lookup = Tbl of (string, data_type) Hashtbl.t

let compute_expression_type (expr: expression): data_type = 



let verify_typing (items: item list) = 


let check_member_naming (items: 'a list) (name_of: 'a -> string): string option =
  let item_name_table = Hashtbl.create ~size:(List.length items) in
  let check_item (i: item): string option = 
    let name = name_of i in
    Option.map ~f:(fun _ -> "Duplicate name " ^ name) (Hashtbl.find item_name_table name) in
  List.find_map ~f:check_item items

let check_variable_naming (items: item list): string option =
  let rec check_function (f: function_definition) : string option = 
    let item_name_table = Hashtbl.create ~size:(List.length items) in
    let check_statement (stmt: statement)  
  in
  List.find_map (function FunctionDefinition f -> check_function f | _ -> None) items
    


let check_naming (items: item list): string option =
  check_member_naming items name_of_item