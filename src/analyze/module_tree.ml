open Ast
open Analyze_error
open Basic_collections

type t = module_tree_node SMap.t

and module_tree_node =
  | Empty of string * t
  | Module of string * t
  | Decl of decl

and decl = {
  name: Identifier.t;
  is_public: bool;
}

let add_decls module_ submodule_tree =
  let open Ast.Module in
  let rec add_decls_inner toplevels =
    (* Add all decls to the submodule tree, given a list of all decls in a list of
       (id, loc, mutator fn) tuples. *)
    let add_decls_to_tree rest decls =
      let (submodule_tree, errors) = add_decls_inner rest in
      List.fold_left
        (fun (submodule_tree, errors) ((decl, loc) : decl * Loc.t) ->
          let name = decl.name.name in
          match SMap.find_opt name submodule_tree with
          | None
          | Some (Decl _) ->
            (* May overwrite decl with same name, but error will be detected during name resolution *)
            (SMap.add name (Decl decl) submodule_tree, errors)
          | Some (Module _ | Empty _) ->
            (* Error for decl with same name as module *)
            let {
              name = { Module.Name.name = { Ast.ScopedIdentifier.name = name_ident; scopes; _ }; _ };
              _;
            } =
              module_
            in
            let scopes_string = Ast_utils.string_of_name_parts (scopes @ [name_ident]) in
            (submodule_tree, (loc, ModuleAndDeclDuplicateNames (name, scopes_string)) :: errors))
        (submodule_tree, errors)
        decls
    in
    match toplevels with
    | [] -> (submodule_tree, [])
    (* Methods declarations do not define any top level declarations *)
    | TraitDeclaration { kind = Methods; _ } :: rest -> add_decls_to_tree rest []
    | VariableDeclaration { Ast.Statement.VariableDeclaration.loc; pattern; is_public; _ } :: rest
      ->
      let ids = Ast_utils.ids_of_pattern pattern in
      let decls = List.map (fun id -> ({ name = id; is_public }, loc)) ids in
      add_decls_to_tree rest decls
    | FunctionDeclaration { Ast.Function.loc; name; is_public; _ } :: rest ->
      add_decls_to_tree rest [({ name; is_public }, loc)]
    | TypeDeclaration { Ast.TypeDeclaration.loc; name; decl; is_public; _ } :: rest ->
      let open Ast.TypeDeclaration in
      let decls = [({ name; is_public }, loc)] in
      (* Create decls for all constructors in this type declaration *)
      let decls =
        match decl with
        | None
        | Alias _
        | Tuple _
        | Record _ ->
          decls
        | Variant variants ->
          List.fold_left
            (fun decls variant ->
              match variant with
              | EnumVariant ({ loc; _ } as name)
              | TupleVariant { loc; name; _ }
              | RecordVariant { loc; name; _ } ->
                ({ name; is_public }, loc) :: decls)
            decls
            variants
      in
      add_decls_to_tree rest (List.rev decls)
    | TraitDeclaration { loc; name; kind = Trait; is_public; _ } :: rest ->
      add_decls_to_tree rest [({ name; is_public }, loc)]
  in
  add_decls_inner module_.toplevels

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
    | (Some (Decl _), _) ->
      ( SMap.empty,
        [
          ( loc,
            ModuleAndDeclDuplicateNames
              (current_part.name, Ast_utils.string_of_name_parts (List.rev prev_name_parts)) );
        ] )
    | (Some (Module _), []) ->
      (SMap.empty, [(loc, DuplicateModuleNames (Ast_utils.string_of_name_parts module_name_parts))])
    | (Some (Empty (_, submodule_tree)), []) ->
      let (submodule_tree, errors) = add_decls module_ submodule_tree in
      let new_module_node = Module (current_part.name, submodule_tree) in
      (SMap.add current_part.name new_module_node module_tree, errors)
    (* Create submodule leaf node *)
    | (None, []) ->
      let (submodule_tree, errors) = add_decls module_ SMap.empty in
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
  | LookupResultDecl of decl
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
      | Some (Decl decl) when rest_parts = [] -> LookupResultDecl decl
      | Some (Decl _) ->
        LookupResultError (loc, ReferenceChildOfDecl (current_name, List.rev prev_name_parts))
      | Some (Module (name, module_tree) | Empty (name, module_tree)) ->
        lookup_inner (name :: prev_name_parts) rest_parts module_tree)
  in
  lookup_inner [] name_parts module_tree

let get_all_decls module_tree =
  let rec get_all_decls_of_node module_tree_node prev_module_parts =
    match module_tree_node with
    | Decl decl ->
      let module_parts = List.rev (List.tl prev_module_parts) in
      [(decl, module_parts)]
    | Empty (_, module_tree)
    | Module (_, module_tree) ->
      SMap.fold
        (fun module_name module_tree_node decls_acc ->
          let decls = get_all_decls_of_node module_tree_node (module_name :: prev_module_parts) in
          decls @ decls_acc)
        module_tree
        []
  in
  SMap.fold
    (fun module_name module_tree_node decls_acc ->
      let decls = get_all_decls_of_node module_tree_node [module_name] in
      decls @ decls_acc)
    module_tree
    []

let analyze existing_module_tree new_modules =
  List.fold_left
    (fun (module_tree, errors) module_ ->
      match add_to_module_tree module_ module_tree with
      | (module_tree, new_errors) -> (module_tree, new_errors @ errors))
    (existing_module_tree, [])
    new_modules
