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
  | Export of unit Ast.Identifier.t

let gather_exports module_ submodules =
  let open Ast.Module in
  let rec gather_exports_inner toplevels =
    match toplevels with
    | [] -> (SMap.empty, [])
    | VariableDeclaration { Ast.Statement.VariableDeclaration.loc; pattern; _ } :: rest ->
      let ({ Ast.Identifier.name; _ } as id) = identifier_in_pattern pattern in
      let (exports, errors) = gather_exports_inner rest in
      if SMap.mem name submodules then
        let {
          module_ = { Module.name = { Ast.ScopedIdentifier.name = name_ident; scopes; _ }; _ };
          _;
        } =
          module_
        in
        let scopes_string = string_of_name_parts (scopes @ [name_ident]) in
        (exports, (loc, ModuleAndExportDuplicateNames (name, scopes_string)) :: errors)
      else
        (SMap.add name (Export id) exports, errors)
    | FunctionDeclaration { Ast.Function.loc; name = { Ast.Identifier.name; _ } as id; _ } :: rest
      ->
      let (exports, errors) = gather_exports_inner rest in
      if SMap.mem name submodules then
        let {
          module_ = { Module.name = { Ast.ScopedIdentifier.name = name_ident; scopes; _ }; _ };
          _;
        } =
          module_
        in
        let scopes_string = string_of_name_parts (scopes @ [name_ident]) in
        (exports, (loc, ModuleAndExportDuplicateNames (name, scopes_string)) :: errors)
      else
        (SMap.add name (Export id) exports, errors)
  in
  gather_exports_inner module_.toplevels

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
      let submodule_names =
        SMap.filter
          (fun _ node ->
            match node with
            | Module _ -> true
            | _ -> false)
          submodule_tree
      in
      let (exports, errors) = gather_exports module_ submodule_names in
      let new_module_node = Module (current_part.name, exports) in
      (SMap.add current_part.name new_module_node module_tree, errors)
    (* Create submodule leaf node *)
    | (None, []) ->
      let (exports, errors) = gather_exports module_ SMap.empty in
      let new_module_node = Module (current_part.name, exports) in
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
