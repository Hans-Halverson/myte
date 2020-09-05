open Analyze_error
open Ast
open Basic_collections

let identifier_in_pattern pat =
  let open Pattern in
  match pat with
  | Identifier id -> id

let string_of_name_parts name_parts =
  name_parts |> List.map (fun { Ast.Identifier.name; _ } -> name) |> String.concat "."

type module_tree = module_tree_node SMap.t

and module_tree_node =
  | Empty of string * module_tree
  | Module of string * module_tree
  | Export of export_info

and export_info = {
  value: Ast.Identifier.t option;
  ty: Ast.Identifier.t option;
}

let add_exports module_ submodule_tree =
  let open Ast.Module in
  let rec add_exports_inner toplevels =
    let add_export ~is_type id loc rest =
      let { Ast.Identifier.name; _ } = id in
      let (submodule_tree, errors) = add_exports_inner rest in
      match SMap.find_opt name submodule_tree with
      | None ->
        let export_info =
          if is_type then
            { value = None; ty = Some id }
          else
            { value = Some id; ty = None }
        in
        (SMap.add name (Export export_info) submodule_tree, errors)
      | Some (Export export_info) ->
        let export_info =
          if is_type then
            { export_info with ty = Some id }
          else
            { export_info with value = Some id }
        in
        (SMap.add name (Export export_info) submodule_tree, errors)
      | Some (Module _ | Empty _) ->
        (* Error for export with same name as module *)
        let {
          module_ = { Module.name = { Ast.ScopedIdentifier.name = name_ident; scopes; _ }; _ };
          _;
        } =
          module_
        in
        let scopes_string = string_of_name_parts (scopes @ [name_ident]) in
        (submodule_tree, (loc, ModuleAndExportDuplicateNames (name, scopes_string)) :: errors)
    in
    match toplevels with
    | [] -> (submodule_tree, [])
    | VariableDeclaration { Ast.Statement.VariableDeclaration.loc; pattern; _ } :: rest ->
      let id = identifier_in_pattern pattern in
      add_export ~is_type:false id loc rest
    | FunctionDeclaration { Ast.Function.loc; name = id; _ } :: rest ->
      add_export ~is_type:false id loc rest
    | TypeDeclaration { Ast.TypeDeclaration.loc; name = id; _ } :: rest ->
      add_export ~is_type:true id loc rest
  in
  add_exports_inner module_.toplevels

let add_to_module_tree module_ module_tree =
  let {
    Ast.Module.module_ =
      { Ast.Module.Module.loc; name = { Ast.ScopedIdentifier.scopes; name; _ }; _ };
    _;
  } =
    module_
  in
  let module_name_parts = scopes @ [name] in
  let rec add_to_module_tree_inner prev_name_parts name_parts module_tree =
    let open Ast.Identifier in
    let current_part = List.hd name_parts in
    let rest_parts = List.tl name_parts in
    match (SMap.find_opt current_part.name module_tree, rest_parts) with
    | (Some (Export _), _) ->
      ( SMap.empty,
        [
          ( loc,
            ModuleAndExportDuplicateNames
              (current_part.name, string_of_name_parts (List.rev prev_name_parts)) );
        ] )
    | (Some (Module _), []) ->
      (SMap.empty, [(loc, DuplicateModuleNames (string_of_name_parts module_name_parts))])
    | (Some (Empty (_, submodule_tree)), []) ->
      let (submodule_tree, errors) = add_exports module_ submodule_tree in
      let new_module_node = Module (current_part.name, submodule_tree) in
      (SMap.add current_part.name new_module_node module_tree, errors)
    (* Create submodule leaf node *)
    | (None, []) ->
      let (submodule_tree, errors) = add_exports module_ SMap.empty in
      let new_module_node = Module (current_part.name, submodule_tree) in
      (SMap.add current_part.name new_module_node module_tree, errors)
    (* Submodule path does not exist *)
    | (None, rest_parts) ->
      let (submodule_tree, errors) =
        add_to_module_tree_inner (current_part :: prev_name_parts) rest_parts SMap.empty
      in
      let submodule_node = Empty (current_part.name, submodule_tree) in
      (SMap.add current_part.name submodule_node module_tree, errors)
    | (Some (Empty (_, submodule_tree)), rest_parts) ->
      let (submodule_tree, errors) =
        add_to_module_tree_inner (current_part :: prev_name_parts) rest_parts submodule_tree
      in
      let submodule_node = Empty (current_part.name, submodule_tree) in
      (SMap.add current_part.name submodule_node module_tree, errors)
    (* Submodule path exists *)
    | (Some (Module (_, submodule_tree)), rest_parts) ->
      let (submodule_tree, errors) =
        add_to_module_tree_inner (current_part :: prev_name_parts) rest_parts submodule_tree
      in
      let submodule_node = Module (current_part.name, submodule_tree) in
      (SMap.add current_part.name submodule_node module_tree, errors)
  in
  add_to_module_tree_inner [] module_name_parts module_tree

let analyze modules =
  List.fold_left
    (fun (module_tree, errors) module_ ->
      match add_to_module_tree module_ module_tree with
      | (module_tree, new_errors) -> (module_tree, new_errors @ errors))
    (SMap.empty, [])
    modules

type lookup_result =
  | LookupResultExport of export_info
  | LookupResultModule of string option * module_tree
  | LookupResultError of Loc.t * Analyze_error.t

let lookup name_parts module_tree =
  let rec lookup_inner prev_name_parts name_parts module_tree =
    match name_parts with
    | [] ->
      let prev_name = List.nth_opt prev_name_parts (List.length prev_name_parts - 1) in
      LookupResultModule (prev_name, module_tree)
    | { Ast.Identifier.loc; name = current_name; _ } :: rest_parts ->
      (match SMap.find_opt current_name module_tree with
      | None -> LookupResultError (loc, ImportNonexist (current_name, List.rev prev_name_parts))
      | Some (Export export) when rest_parts = [] -> LookupResultExport export
      | Some (Export _) ->
        LookupResultError (loc, ImportChildOfExport (current_name, List.rev prev_name_parts))
      | Some (Module (name, module_tree) | Empty (name, module_tree)) ->
        lookup_inner (name :: prev_name_parts) rest_parts module_tree)
  in
  lookup_inner [] name_parts module_tree
