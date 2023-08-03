open Core

type environment = {
  function_types : (string, Tsr.data_type) Hashtbl.t;
  aliases : (string, Tsr.data_type) Hashtbl.t;
  scoped_variables : (string, Tsr.data_type) Hashtbl.t;
}

let clone_env env =
  {
    function_types = Hashtbl.copy env.function_types;
    aliases = Hashtbl.copy env.aliases;
    scoped_variables = Hashtbl.copy env.scoped_variables;
  }

let rec map_data_type env dt =
  match dt with
  | Ast.Void _ -> Tsr.Void
  | Ast.Bool _ -> Tsr.Bool
  | Ast.Numeric (format, size, _) ->
      Tsr.Numeric (format, Option.value ~default:8 size)
  | Ast.Tuple (factors, _) ->
      Tsr.Tuple (List.map ~f:(map_data_type env) factors)
  | Ast.Pointer (inner, _) -> Tsr.Pointer (map_data_type env inner)
  | Ast.Named (name, li) ->
      let error =
        Printf.sprintf "no type alias %s in scope %s" name (Ast.print_li li)
      in
      Tsr.Named
        (name, Option.value_exn ~message:error (Hashtbl.find env.aliases name))

let filter_function env i =
  match i with
  | Ast.Function ({ name; return_type; _ }, _) ->
      Some (name, map_data_type env return_type)
  | Ast.Extern ({ name; return_type; _ }, _) ->
      Some (name, map_data_type env return_type)
  | Ast.Type _ -> None

let is_alias i =
  match i with Ast.Type (name, dt, _) -> Some (name, dt) | _ -> None

let rec extract_alias_dependents dt =
  match dt with
  | Ast.Void _ -> []
  | Ast.Bool _ -> []
  | Ast.Numeric _ -> []
  | Ast.Tuple (factors, _) ->
      List.concat (List.map factors ~f:extract_alias_dependents)
  | Ast.Pointer (i, _) -> extract_alias_dependents i
  | Ast.Named (name, _) -> [ name ]

let alias_list_to_map alias_list =
  let alias_map = Hashtbl.of_alist_or_error (module String) alias_list in
  Option.value_exn (Result.ok alias_map) ~message:"duplicate type name"

module TypeAliasGraph = Graph.Imperative.Digraph.Concrete (String)
module SortableTypeAliasGraph = Graph.Topological.Make (TypeAliasGraph)

let update_aliases env items =
  let alias_list = List.filter_map items ~f:is_alias in
  let alias_map = alias_list_to_map alias_list in
  let alias_deps =
    List.map ~f:(fun (name, x) -> (name, extract_alias_dependents x)) alias_list
  in
  let result = TypeAliasGraph.create ~size:(List.length alias_list) () in
  List.iter alias_list ~f:(fun (name, _) ->
      TypeAliasGraph.add_vertex result name);
  let add_edges (name, deps) =
    List.iter deps ~f:(fun d -> TypeAliasGraph.add_edge result name d)
  in
  List.iter alias_deps ~f:add_edges;
  let insert_mapped_type name =
    let data_type = map_data_type env (Hashtbl.find_exn alias_map name) in
    let _ = Hashtbl.set env.aliases ~key:name ~data:data_type in
    ()
  in
  let order =
    SortableTypeAliasGraph.fold (fun n l -> List.cons n l) result []
  in
  List.iter ~f:insert_mapped_type order;
  ()

let build_environment items =
  let env =
    {
      function_types = Hashtbl.create (module String);
      aliases = Hashtbl.create (module String);
      scoped_variables = Hashtbl.create (module String);
    }
  in
  update_aliases env items;
  let function_types = List.filter_map ~f:(filter_function env) items in
  let function_types =
    Hashtbl.of_alist_or_error (module String) function_types
  in
  let function_types =
    Option.value_exn ~message:"duplicate function in module"
      (Result.ok function_types)
  in
  { env with function_types }
