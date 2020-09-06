open Analyze_error
open Ast
open Basic_collections

type value_declaration =
  | VarDecl
  | FunDecl
  | ImportedValue
  | ImportedModule of Module_tree.module_tree
  | FunParam

type type_declaration =
  | TypeDecl
  | ImportedType
  | ImportedModule of Module_tree.module_tree

module ValueBinding = struct
  type t = {
    name: string;
    declaration: Loc.t * value_declaration;
    uses: LocSet.t;
  }
end

module TypeBinding = struct
  type t = {
    name: string;
    declaration: Loc.t * type_declaration;
    uses: LocSet.t;
  }
end

type t = {
  use_to_binding: ValueBinding.t LocMap.t;
  declaration_to_binding: ValueBinding.t LocMap.t;
}

type scope = {
  local_values: Loc.t SMap.t;
  local_types: Loc.t SMap.t;
}

type scopes = scope list

type 'a binding_builder = {
  name: string;
  declaration: Loc.t * 'a;
  mutable uses: LocSet.t;
}

let identifier_in_pattern pat =
  let open Pattern in
  match pat with
  | Identifier id -> id

let map = Ast_mapper.map

let map_list = Ast_mapper.map_list

class bindings_builder ~module_tree =
  object (this)
    inherit Ast_mapper.mapper as super

    val mutable value_bindings : value_declaration binding_builder LocMap.t = LocMap.empty

    val mutable type_bindings : type_declaration binding_builder LocMap.t = LocMap.empty

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : scopes = []

    method add_error loc err = errors <- (loc, err) :: errors

    method add_value_declaration loc kind name =
      value_bindings <-
        LocMap.add loc { name; declaration = (loc, kind); uses = LocSet.empty } value_bindings;
      match scopes with
      | [] -> failwith "There must always be a scope"
      | { local_values; local_types } :: rest ->
        scopes <- { local_values = SMap.add name loc local_values; local_types } :: rest

    method add_type_declaration loc kind name =
      type_bindings <-
        LocMap.add loc { name; declaration = (loc, kind); uses = LocSet.empty } type_bindings;
      match scopes with
      | [] -> failwith "There must always be a scope"
      | { local_values; local_types } :: rest ->
        scopes <- { local_types = SMap.add name loc local_types; local_values } :: rest

    method add_value_use declaration use =
      let binding = LocMap.find declaration value_bindings in
      binding.uses <- LocSet.add use binding.uses

    method add_type_use declaration use =
      let binding = LocMap.find declaration type_bindings in
      binding.uses <- LocSet.add use binding.uses

    method enter_scope () =
      scopes <- { local_values = SMap.empty; local_types = SMap.empty } :: scopes

    method exit_scope () = scopes <- List.tl scopes

    method lookup_value_in_scope name scopes =
      match scopes with
      | [] -> None
      | { local_values; _ } :: rest ->
        (match SMap.find_opt name local_values with
        | None -> this#lookup_value_in_scope name rest
        | Some declaration -> Some declaration)

    method lookup_type_in_scope name scopes =
      match scopes with
      | [] -> None
      | { local_types; _ } :: rest ->
        (match SMap.find_opt name local_types with
        | None -> this#lookup_type_in_scope name rest
        | Some declaration -> Some declaration)

    method results () =
      let bindings =
        LocMap.map
          (fun { name; declaration; uses } -> { ValueBinding.name; declaration; uses })
          value_bindings
      in
      let errors = List.rev errors in
      (bindings, errors)

    method! module_ mod_ =
      let open Ast.Module in
      let add_value_name kind name =
        let { Ast.Identifier.loc; name } = name in
        let is_duplicate = SMap.mem name (List.hd scopes).local_values in
        if is_duplicate then this#add_error loc (DuplicateToplevelNames (name, true));
        this#add_value_declaration loc kind name;
        is_duplicate
      in
      let add_type_name kind name =
        let { Ast.Identifier.loc; name } = name in
        if SMap.mem name (List.hd scopes).local_types then
          this#add_error loc (DuplicateToplevelNames (name, false));
        this#add_type_declaration loc kind name
      in
      let { toplevels; imports; _ } = mod_ in
      this#enter_scope ();
      (* Gather imports and add them to toplevel scope *)
      List.iter
        (fun import ->
          let open Import in
          let resolve_import name local_name scopes =
            let open Module_tree in
            let name_parts = scopes @ [name] in
            match lookup name_parts module_tree with
            | LookupResultExport export_info ->
              ignore
                (Option.map (fun _ -> add_value_name ImportedValue local_name) export_info.value);
              Option.iter (fun _ -> add_type_name ImportedType local_name) export_info.ty
            | LookupResultModule (_, module_tree) ->
              (* Do not report a duplicate error for both value and type conflicting with a module *)
              if not (add_value_name (ImportedModule module_tree) local_name) then
                add_type_name (ImportedModule module_tree) local_name
            | LookupResultError (loc, error) -> this#add_error loc error
            (* TODO: Figure out error case*)
            (* ImportedExport { Module_tree.value = None; ty = None } *)
          in
          match import with
          | Simple { name; scopes; _ } ->
            (* Add name to toplevel scope *)
            resolve_import name name scopes
          | Complex { Complex.aliases; scopes; _ } ->
            (* Add local names to toplevel scope *)
            List.iter
              (fun alias ->
                match alias with
                | { Alias.name; alias = Some local_name; _ }
                | { Alias.name = _ as name as local_name; _ } ->
                  resolve_import name local_name scopes)
              aliases)
        imports;
      (* Gather toplevel declarations add add them to toplevel scope *)
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.pattern; _ } ->
            let id = identifier_in_pattern pattern in
            ignore (add_value_name VarDecl id)
          | FunctionDeclaration { Ast.Function.name; _ } -> ignore (add_value_name FunDecl name)
          | TypeDeclaration { Ast.TypeDeclaration.name; _ } -> add_type_name TypeDecl name)
        toplevels;
      (* Then visit child nodes once toplevel scope is complete *)
      let toplevels' =
        map_list
          (fun toplevel ->
            match toplevel with
            | VariableDeclaration decl ->
              map (this#visit_variable_declaration ~add:false) decl toplevel (fun decl' ->
                  VariableDeclaration decl')
            | FunctionDeclaration decl ->
              map (this#visit_function_declaration ~add:false) decl toplevel (fun decl' ->
                  FunctionDeclaration decl')
            | TypeDeclaration _ -> toplevel)
          toplevels
      in
      this#exit_scope ();
      if toplevels == toplevels' then
        mod_
      else
        { mod_ with toplevels = toplevels' }

    method! statement stmt =
      let open Ast.Statement in
      match stmt with
      | VariableDeclaration decl ->
        map (this#visit_variable_declaration ~add:true) decl stmt (fun decl' ->
            VariableDeclaration decl')
      | FunctionDeclaration decl ->
        map (this#visit_function_declaration ~add:true) decl stmt (fun decl' ->
            FunctionDeclaration decl')
      | Block block -> map this#block block stmt (fun block' -> Block block')
      | _ -> super#statement stmt

    method! block block =
      this#enter_scope ();
      let block' = super#block block in
      this#exit_scope ();
      block'

    method visit_variable_declaration ~add decl =
      let { Ast.Statement.VariableDeclaration.pattern; _ } = decl in
      let id = identifier_in_pattern pattern in
      let { Ast.Identifier.loc; name; _ } = id in
      if add then this#add_value_declaration loc VarDecl name;
      super#variable_declaration decl

    method visit_function_declaration ~add decl =
      let open Ast.Function in
      let { name = { Ast.Identifier.loc; name = func_name; _ }; params; _ } = decl in
      if add then this#add_value_declaration loc FunDecl func_name;
      this#enter_scope ();
      let _ =
        List.fold_left
          (fun param_names { Param.name = { Ast.Identifier.loc; name; _ }; _ } ->
            if SSet.mem name param_names then
              this#add_error loc (DuplicateParameterNames (name, func_name));
            this#add_value_declaration loc FunParam name;
            SSet.add name param_names)
          SSet.empty
          params
      in
      let function_ = super#function_ decl in
      this#exit_scope ();
      function_

    (* Match a sequence of module parts against the module tree, returning the same AST with the
       matched access chain replaced with a scoped id if a match exists. Otherwise error. *)
    method match_module_parts ~is_value module_tree prev_parts rest_parts prev_is_module on_export =
      let open Ast.Identifier in
      let open Module_tree in
      match rest_parts with
      | [] -> failwith "There must be at least two parts in a scoped identifier"
      | ({ name; loc; _ } as part) :: rest_parts ->
        (match (SMap.find_opt name module_tree, rest_parts) with
        | (None, _)
        | (Some (Empty _), []) ->
          (* Error on no match - but check if parent module exists for better error message *)
          let full_loc = Loc.between (List.hd prev_parts).loc loc in
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
          if prev_is_module then
            this#add_error full_loc (NoExportInModule (name, prev_parts_names, is_value))
          else
            this#add_error full_loc (NoModuleWithName (prev_parts_names @ [name], is_value));
          None
        | (Some (Module _), []) ->
          (* Error if resolved to module as modules are not types or values *)
          let full_loc = Loc.between (List.hd prev_parts).loc loc in
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
          this#add_error full_loc (ModuleInvalidPosition (prev_parts_names @ [name], is_value));
          None
        | (Some (Export _), _) -> on_export prev_parts part rest_parts
        | (Some (Empty (_, module_tree)), rest_parts) ->
          this#match_module_parts
            ~is_value
            module_tree
            (prev_parts @ [part])
            rest_parts
            false
            on_export
        | (Some (Module (_, module_tree)), rest_parts) ->
          this#match_module_parts
            ~is_value
            module_tree
            (prev_parts @ [part])
            rest_parts
            true
            on_export)

    method match_module_parts_value module_tree prev_parts rest_parts expr =
      this#match_module_parts
        ~is_value:true
        module_tree
        prev_parts
        rest_parts
        false
        (fun prev_parts ({ Ast.Identifier.loc; _ } as part) rest_parts ->
          (* Resolved to export - convert nested accesses to scoped identifier *)
          let full_loc = Loc.between (List.hd prev_parts).loc loc in
          let scoped_id =
            Ast.Expression.ScopedIdentifier
              { Ast.ScopedIdentifier.loc = full_loc; name = part; scopes = prev_parts }
          in
          (* Make sure new scoped id only replaces accesses that were matched, using number of
             unmatched accesses to know how how many to preserve. *)
          let rec insert_scoped_id expr depth scoped_id =
            match expr with
            | Ast.Expression.Access ({ left; _ } as access) ->
              if depth = 0 then
                scoped_id
              else
                Ast.Expression.Access
                  { access with left = insert_scoped_id left (depth - 1) scoped_id }
            | _ -> failwith "Must be nested access expression"
          in
          Some (insert_scoped_id expr (List.length rest_parts) scoped_id))

    method match_module_parts_type module_tree first_part rest_parts =
      ignore
        (this#match_module_parts
           ~is_value:false
           module_tree
           [first_part]
           rest_parts
           false
           (fun _ _ _ -> None))

    method! expression expr =
      let open Ast.Expression in
      match expr with
      | Identifier { loc; name; _ } ->
        (match this#lookup_value_in_scope name scopes with
        | None -> this#add_error loc (UnresolvedName (name, true))
        | Some decl_loc ->
          let (_, declaration) = (LocMap.find decl_loc value_bindings).declaration in
          (match declaration with
          | ImportedModule _ -> this#add_error loc (ModuleInvalidPosition ([name], true))
          | _ -> this#add_value_use decl_loc loc));
        expr
      | Access { left; right; _ } ->
        (* Gather all potential module parts in order if there is an unbroken chain of accesses
           ending in an id *)
        let rec gather_potential_module_parts expr parts =
          match expr with
          | Identifier id -> Some (id :: parts)
          | Access { left; right; _ } -> gather_potential_module_parts left (right :: parts)
          | _ -> None
        in
        let parts = gather_potential_module_parts left [right] in
        (match parts with
        | None -> super#expression expr
        | Some parts ->
          let open Ast.Identifier in
          let first_part = List.hd parts in
          let rest_parts = List.tl parts in
          (match this#lookup_value_in_scope first_part.name scopes with
          | None ->
            (match SMap.find_opt first_part.name module_tree with
            | None ->
              (* Error if first part of access chain cannot be resolved *)
              this#add_error first_part.loc (UnresolvedName (first_part.name, true));
              expr
            | Some (Export _) -> failwith "Exports cannot appear at top level of module tree"
            | Some (Empty (_, module_tree) | Module (_, module_tree)) ->
              (* If some portion of the access chain resolves to an export, replace with scoped id *)
              (match this#match_module_parts_value module_tree [first_part] rest_parts expr with
              | None -> expr
              | Some resolved_ast -> resolved_ast))
          | Some decl_loc ->
            this#add_value_use decl_loc first_part.loc;
            let (_, declaration) = (LocMap.find decl_loc value_bindings).declaration in
            (match declaration with
            | ImportedModule module_tree ->
              (match this#match_module_parts_value module_tree [first_part] rest_parts expr with
              | None -> expr
              | Some resolved_ast -> resolved_ast)
            | _ -> expr)))
      | _ -> super#expression expr

    method! type_ ty =
      let open Ast.Type in
      match ty with
      | Custom { name = { Ast.ScopedIdentifier.name; scopes = scope_ids; _ }; _ } ->
        let all_parts = scope_ids @ [name] in
        let open Ast.Identifier in
        let first_part = List.hd all_parts in
        let rest_parts = List.tl all_parts in
        (match this#lookup_type_in_scope first_part.name scopes with
        | None ->
          (match SMap.find_opt first_part.name module_tree with
          | None ->
            (* Error if first part of scoped id cannot be resolved *)
            this#add_error first_part.loc (UnresolvedName (first_part.name, false));
            ty
          | Some (Export _) -> failwith "Exports cannot appear at top level of module tree"
          | Some (Empty (_, module_tree) | Module (_, module_tree)) ->
            (match rest_parts with
            | [] ->
              let { Ast.Identifier.loc; name } = first_part in
              this#add_error loc (ModuleInvalidPosition ([name], false))
            | _ :: _ -> this#match_module_parts_type module_tree first_part rest_parts);
            ty)
        | Some decl_loc ->
          this#add_type_use decl_loc first_part.loc;
          let (_, declaration) = (LocMap.find decl_loc type_bindings).declaration in
          (match declaration with
          | ImportedModule module_tree ->
            (match rest_parts with
            | [] ->
              let { Ast.Identifier.loc; name } = first_part in
              this#add_error loc (ModuleInvalidPosition ([name], false))
            | _ :: _ -> this#match_module_parts_type module_tree first_part rest_parts);
            ty
          | _ -> ty))
      | _ -> super#type_ ty
  end

let analyze modules module_tree =
  let results =
    List.map
      (fun (file, mod_) ->
        let bindings_builder = new bindings_builder ~module_tree in
        let mod' = bindings_builder#module_ mod_ in
        let (bindings, bindings_errors) = bindings_builder#results () in
        ((file, mod'), bindings, bindings_errors))
      modules
  in
  let (modules', bindings, bindings_errors) = List_utils.split3 results in
  let bindings =
    List.fold_left (fun bindings acc -> LocMap.fold LocMap.add bindings acc) LocMap.empty bindings
  in
  let errors = List.flatten bindings_errors in
  (modules', bindings, errors)
