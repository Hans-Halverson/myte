open Ast
open Analyze_error
open Basic_collections

type t = module_tree_node SMap.t

and module_tree_node =
  | Empty of string * t
  | Module of string * t
  | Export of Identifier.t

let add_exports module_ submodule_tree =
  let open Ast.Module in
  let rec add_exports_inner toplevels =
    (* Add all exports to the submodule tree, given a list of all exports in a list of
       (id, loc, mutator fn) tuples. *)
    let add_exports_to_tree rest exports =
      let (submodule_tree, errors) = add_exports_inner rest in
      List.fold_left
        (fun (submodule_tree, errors) (id, loc) ->
          let { Ast.Identifier.name; _ } = id in
          match SMap.find_opt name submodule_tree with
          | None
          | Some (Export _) ->
            (* May overwrite export with same name, but error will be detected during name resolution *)
            (SMap.add name (Export id) submodule_tree, errors)
          | Some (Module _ | Empty _) ->
            (* Error for export with same name as module *)
            let {
              name = { Module.Name.name = { Ast.ScopedIdentifier.name = name_ident; scopes; _ }; _ };
              _;
            } =
              module_
            in
            let scopes_string = Ast_utils.string_of_name_parts (scopes @ [name_ident]) in
            (submodule_tree, (loc, ModuleAndExportDuplicateNames (name, scopes_string)) :: errors))
        (submodule_tree, errors)
        exports
    in
    match toplevels with
    | [] -> (submodule_tree, [])
    (* Methods declarations do not define any exports *)
    | TraitDeclaration { kind = Methods; _ } :: rest -> add_exports_to_tree rest []
    | VariableDeclaration { Ast.Statement.VariableDeclaration.loc; pattern; _ } :: rest ->
      let ids = Ast_utils.ids_of_pattern pattern in
      let exports = List.map (fun id -> (id, loc)) ids in
      add_exports_to_tree rest exports
    | FunctionDeclaration { Ast.Function.loc; name = id; _ } :: rest ->
      add_exports_to_tree rest [(id, loc)]
    | TypeDeclaration { Ast.TypeDeclaration.loc; name = id; decl; _ } :: rest ->
      let open Ast.TypeDeclaration in
      let exports = [(id, loc)] in
      (* Export all constructors in this type declaration *)
      let exports =
        match decl with
        | Builtin
        | Alias _
        | Tuple _
        | Record _ ->
          exports
        | Variant variants ->
          List.fold_left
            (fun exports variant ->
              match variant with
              | EnumVariant ({ loc; _ } as name)
              | TupleVariant { loc; name; _ }
              | RecordVariant { loc; name; _ } ->
                (name, loc) :: exports)
            exports
            variants
      in
      add_exports_to_tree rest (List.rev exports)
    | TraitDeclaration { loc; name; kind = Trait; _ } :: rest ->
      add_exports_to_tree rest [(name, loc)]
  in
  add_exports_inner module_.toplevels

let add_to_module_tree module_ module_tree =
  let {
    Ast.Module.name = { Ast.Module.Name.loc; name = { Ast.ScopedIdentifier.scopes; name; _ }; _ };
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
              (current_part.name, Ast_utils.string_of_name_parts (List.rev prev_name_parts)) );
        ] )
    | (Some (Module _), []) ->
      (SMap.empty, [(loc, DuplicateModuleNames (Ast_utils.string_of_name_parts module_name_parts))])
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

type lookup_result =
  | LookupResultExport of Identifier.t
  | LookupResultModule of string option * t
  | LookupResultError of Analyze_error.error

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
        LookupResultError (loc, ReferenceChildOfExport (current_name, List.rev prev_name_parts))
      | Some (Module (name, module_tree) | Empty (name, module_tree)) ->
        lookup_inner (name :: prev_name_parts) rest_parts module_tree)
  in
  lookup_inner [] name_parts module_tree

let get_all_exports module_tree =
  let rec get_all_exports_of_node module_tree_node prev_module_parts =
    match module_tree_node with
    | Export id ->
      let module_parts = List.rev (List.tl prev_module_parts) in
      [(id, module_parts)]
    | Empty (_, module_tree)
    | Module (_, module_tree) ->
      SMap.fold
        (fun module_name module_tree_node exports_acc ->
          let exports =
            get_all_exports_of_node module_tree_node (module_name :: prev_module_parts)
          in
          exports @ exports_acc)
        module_tree
        []
  in
  SMap.fold
    (fun module_name module_tree_node exports_acc ->
      let exports = get_all_exports_of_node module_tree_node [module_name] in
      exports @ exports_acc)
    module_tree
    []

let analyze existing_module_tree new_modules =
  List.fold_left
    (fun (module_tree, errors) module_ ->
      match add_to_module_tree module_ module_tree with
      | (module_tree, new_errors) -> (module_tree, new_errors @ errors))
    (existing_module_tree, [])
    new_modules
