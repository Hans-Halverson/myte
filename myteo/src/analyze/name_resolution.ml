open Analyze_error
open Ast
open Basic_collections
open Bindings
open Immutable_utils

type scope = {
  local_values: Loc.t SMap.t;
  local_types: Loc.t SMap.t;
}

type scopes = scope list

type 'a builder_decl =
  | Decl of 'a
  | ModuleDecl of Module_tree.t

type 'a binding_builder = {
  name: string;
  loc: Loc.t;
  declaration: 'a builder_decl;
  mutable uses: LocSet.t;
  is_global: bool;
  module_: string list;
}

let mk_binding_builder ~loc ~name ~kind ~is_global ~module_ =
  { name; loc; declaration = kind; uses = LocSet.empty; is_global; module_ }

class bindings_builder ~is_stdlib ~module_tree =
  let (exported_value_ids, exported_type_ids) = Module_tree.get_all_exports module_tree in
  let exported_value_bindings =
    List.fold_left
      (fun acc (kind, { Ast.Identifier.loc; name }, module_) ->
        let kind =
          match kind with
          | Module_tree.VarDecl kind -> Decl (VarDecl (VariableDeclaration.mk kind))
          | Module_tree.FunDecl is_builtin -> Decl (FunDecl (FunctionDeclaration.mk is_builtin))
          | Module_tree.CtorDecl -> Decl (CtorDecl (ConstructorDeclaration.mk ()))
        in
        LocMap.add loc (mk_binding_builder ~loc ~name ~kind ~is_global:true ~module_) acc)
      LocMap.empty
      exported_value_ids
  in
  let exported_type_bindings =
    List.fold_left
      (fun acc (kind, { Ast.Identifier.loc; name }, module_) ->
        let kind =
          match kind with
          | Module_tree.TypeDecl -> Decl (TypeDecl (TypeDeclaration.mk ()))
          | Module_tree.TypeAlias alias_decl -> Decl (TypeAlias alias_decl)
        in
        LocMap.add loc (mk_binding_builder ~loc ~name ~kind ~is_global:true ~module_) acc)
      LocMap.empty
      exported_type_ids
  in
  object (this)
    inherit Ast_mapper.mapper as super

    val mutable value_bindings : value_declaration binding_builder LocMap.t =
      exported_value_bindings

    val mutable type_bindings : type_declaration binding_builder LocMap.t = exported_type_bindings

    val mutable value_use_to_decl : Loc.t LocMap.t = LocMap.empty

    val mutable type_use_to_decl : Loc.t LocMap.t = LocMap.empty

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : scopes = []

    val mutable module_name : string list = []

    method add_error loc err = errors <- (loc, err) :: errors

    method add_value_to_scope name decl_loc =
      match scopes with
      | [] -> failwith "There must always be a scope"
      | { local_values; local_types } :: rest ->
        scopes <- { local_values = SMap.add name decl_loc local_values; local_types } :: rest

    method add_type_to_scope name decl_loc =
      match scopes with
      | [] -> failwith "There must always be a scope"
      | { local_values; local_types } :: rest ->
        scopes <- { local_types = SMap.add name decl_loc local_types; local_values } :: rest

    method add_value_declaration loc name is_global mk_decl =
      ( if not (LocMap.mem loc value_bindings) then
        let declaration = mk_decl () in
        let binding =
          { name; loc; declaration; uses = LocSet.empty; is_global; module_ = module_name }
        in
        value_bindings <- LocMap.add loc binding value_bindings );
      this#add_value_to_scope name loc;
      this#add_value_use loc loc

    method add_type_declaration loc name mk_decl =
      ( if not (LocMap.mem loc type_bindings) then
        let declaration = mk_decl () in
        let binding =
          { name; loc; declaration; uses = LocSet.empty; is_global = true; module_ = module_name }
        in

        type_bindings <- LocMap.add loc binding type_bindings );
      this#add_type_to_scope name loc;
      this#add_type_use loc loc

    method add_value_use declaration use =
      let binding = LocMap.find declaration value_bindings in
      binding.uses <- LocSet.add use binding.uses;
      value_use_to_decl <- LocMap.add use declaration value_use_to_decl

    method add_type_use declaration use =
      let binding = LocMap.find declaration type_bindings in
      binding.uses <- LocSet.add use binding.uses;
      type_use_to_decl <- LocMap.add use declaration type_use_to_decl

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
      let value_bindings =
        LocMap.fold
          (fun key_loc { name; loc; declaration; uses; is_global; module_ } acc ->
            match declaration with
            | Decl declaration ->
              let binding = { ValueBinding.name; loc; declaration; uses; is_global; module_ } in
              LocMap.add key_loc binding acc
            | ModuleDecl _ -> acc)
          value_bindings
          LocMap.empty
      in
      let type_bindings =
        LocMap.fold
          (fun key_loc { name; loc; declaration; uses; module_; _ } acc ->
            match declaration with
            | Decl declaration ->
              let binding = { TypeBinding.name; loc; declaration; uses; module_ } in
              LocMap.add key_loc binding acc
            | ModuleDecl _ -> acc)
          type_bindings
          LocMap.empty
      in
      let errors = List.rev errors in
      ({ Bindings.value_bindings; type_bindings; value_use_to_decl; type_use_to_decl }, errors)

    method! module_ mod_ =
      let open Ast.Module in
      let check_duplicate_value_name name =
        let { Ast.Identifier.loc; name } = name in
        let is_duplicate = SMap.mem name (List.hd scopes).local_values in
        if is_duplicate then this#add_error loc (DuplicateToplevelNames (name, true));
        is_duplicate
      in
      let check_duplicate_type_name name =
        let { Ast.Identifier.loc; name } = name in
        if SMap.mem name (List.hd scopes).local_types then
          this#add_error loc (DuplicateToplevelNames (name, false))
      in
      let add_value_name name mk_decl =
        let is_duplicate = check_duplicate_value_name name in
        this#add_value_declaration name.loc name.name true mk_decl;
        is_duplicate
      in
      let add_type_name name mk_decl =
        check_duplicate_type_name name;
        this#add_type_declaration name.loc name.name mk_decl
      in
      let { module_; toplevels; imports; _ } = mod_ in
      module_name <- Ast_utils.name_parts_of_scoped_ident module_.name;
      let module_name_prefix = String.concat "." module_name ^ "." in
      this#enter_scope ();
      (* Add all implicit imports to toplevel scope *)
      ( if not is_stdlib then
        let open Std_lib in
        this#add_type_to_scope "String" (lookup_stdlib_decl_loc std_string_string) );
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
                (Option.map
                   (fun (_, { Ast.Identifier.loc = decl_loc; _ }) ->
                     ignore (check_duplicate_value_name local_name);
                     this#add_value_to_scope local_name.name decl_loc)
                   export_info.value);
              Option.iter
                (fun (_, { Ast.Identifier.loc = decl_loc; _ }) ->
                  check_duplicate_type_name local_name;
                  this#add_type_to_scope local_name.name decl_loc)
                export_info.ty
            | LookupResultModule (_, module_tree) ->
              (* Do not report a duplicate error for both value and type conflicting with a module *)
              if not (add_value_name local_name (fun _ -> ModuleDecl module_tree)) then
                add_type_name local_name (fun _ -> ModuleDecl module_tree)
            | LookupResultError (loc, error) -> this#add_error loc error
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
      (* Gather toplevel type and variable declarations and add them to toplevel scope *)
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.kind; pattern; _ } ->
            let ids = Ast_utils.ids_of_pattern pattern in
            List.iter
              (fun id ->
                ignore (add_value_name id (fun _ -> Decl (VarDecl (VariableDeclaration.mk kind)))))
              ids
          | FunctionDeclaration { Ast.Function.name; builtin; _ } ->
            ignore (add_value_name name (fun _ -> Decl (FunDecl (FunctionDeclaration.mk builtin))))
          | TypeDeclaration { Ast.TypeDeclaration.name; decl; _ } ->
            ( if is_stdlib then
              let full_name = module_name_prefix ^ name.name in
              Std_lib.register_stdlib_decl full_name name.loc );
            if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else (
              (match decl with
              | Tuple { name; _ }
              | Record { name; _ } ->
                ignore
                  (add_value_name name (fun _ -> Decl (CtorDecl (ConstructorDeclaration.mk ()))))
              | _ -> ());
              let mk_decl _ =
                match decl with
                | Alias _ -> Decl (TypeAlias (TypeAliasDeclaration.mk ()))
                | Tuple _
                | Record _
                | Variant _ ->
                  Decl (TypeDecl (TypeDeclaration.mk ()))
              in
              add_type_name name mk_decl
            )
          | MethodsDeclaration _ -> ())
        toplevels;
      (* Add variant type declarations to toplevel scope *)
      List.iter
        (fun toplevel ->
          let open Ast.TypeDeclaration in
          match toplevel with
          | TypeDeclaration { decl = Variant variants; _ } ->
            List.iter
              (fun variant ->
                match variant with
                | EnumVariant name
                | TupleVariant { Tuple.name; _ }
                | RecordVariant { Record.name; _ } ->
                  ignore
                    (add_value_name name (fun _ -> Decl (CtorDecl (ConstructorDeclaration.mk ())))))
              variants
          | _ -> ())
        toplevels;
      (* Then visit child nodes once toplevel scope is complete *)
      let toplevels' =
        id_map_list
          (fun toplevel ->
            match toplevel with
            | VariableDeclaration decl ->
              id_map (this#visit_variable_declaration ~toplevel:true) decl toplevel (fun decl' ->
                  VariableDeclaration decl')
            | FunctionDeclaration decl ->
              id_map (this#visit_function_declaration ~toplevel:true) decl toplevel (fun decl' ->
                  FunctionDeclaration decl')
            | TypeDeclaration
                ( { Ast.TypeDeclaration.name = { Ast.Identifier.name; _ }; type_params; _ } as
                type_decl ) ->
              if type_params <> [] then this#enter_scope ();
              this#add_type_parameter_declarations type_params (TypeName name);
              ignore (this#type_declaration type_decl);
              if type_params <> [] then this#exit_scope ();
              toplevel
            | MethodsDeclaration decl ->
              id_map this#visit_methods_declaration decl toplevel (fun decl' ->
                  MethodsDeclaration decl'))
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
        id_map (this#visit_variable_declaration ~toplevel:false) decl stmt (fun decl' ->
            VariableDeclaration decl')
      | FunctionDeclaration decl ->
        id_map (this#visit_function_declaration ~toplevel:false) decl stmt (fun decl' ->
            FunctionDeclaration decl')
      | Block block -> id_map this#block block stmt (fun block' -> Block block')
      | _ -> super#statement stmt

    method! block block =
      this#enter_scope ();
      let block' = super#block block in
      this#exit_scope ();
      block'

    method visit_variable_declaration ~toplevel decl =
      let { Ast.Statement.VariableDeclaration.kind; pattern; init; annot; loc = _ } = decl in
      let annot' = id_map_opt this#type_ annot in
      let init' = this#expression init in
      let ids = Ast_utils.ids_of_pattern pattern in
      this#visit_pattern ~decl:true ~toplevel pattern;
      List.iter
        (fun { Ast.Identifier.loc; name; _ } ->
          this#add_value_declaration loc name toplevel (fun _ ->
              Decl (VarDecl (VariableDeclaration.mk kind))))
        ids;
      if init == init' && annot == annot' then
        decl
      else
        { decl with annot = annot'; init = init' }

    method add_type_parameter_declarations params source =
      ignore
        ((List.fold_left
            (fun param_names { TypeParameter.name = { Ast.Identifier.loc; name }; _ } ->
              if SSet.mem name param_names then
                this#add_error loc (DuplicateTypeParameterNames (name, source));
              this#add_type_declaration loc name (fun _ ->
                  Decl (TypeParam (TypeParamDeclaration.mk ())));
              SSet.add name param_names)
            SSet.empty)
           params)

    method visit_function_declaration ~toplevel decl =
      let open Ast.Function in
      let { name = { Ast.Identifier.loc; name = func_name }; params; type_params; builtin; _ } =
        decl
      in
      if not toplevel then
        this#add_value_declaration loc func_name false (fun _ ->
            Decl (FunDecl (FunctionDeclaration.mk builtin)));
      this#enter_scope ();
      this#add_type_parameter_declarations type_params (FunctionName func_name);
      let _ =
        List.fold_left
          (fun param_names { Param.name = { Ast.Identifier.loc; name; _ }; _ } ->
            if SSet.mem name param_names then
              this#add_error loc (DuplicateParameterNames (name, func_name));
            this#add_value_declaration loc name false (fun _ ->
                Decl (FunParamDecl (FunctionParamDeclaration.mk ())));
            SSet.add name param_names)
          SSet.empty
          params
      in
      let function_ = super#function_ decl in
      this#exit_scope ();
      function_

    method visit_methods_declaration decl =
      (* TODO: Name resolution for method declaration blocks *)
      super#methods_declaration decl

    method! assignment assign =
      let open Statement.Assignment in
      match assign.lvalue with
      (* LValue expression are resolved like normal expressions *)
      | Expression _ -> super#assignment assign
      (* Resolve every identifier in an LValue pattern to a declaration *)
      | Pattern pattern ->
        let ids = Ast_utils.ids_of_pattern pattern in
        List.iter
          (fun { Identifier.name; _ } -> ignore (this#lookup_value_in_scope name scopes))
          ids;
        super#assignment assign

    (* Match a sequence of module parts against the module tree, returning the same AST with the
       matched access chain replaced with a scoped id if a match exists. Otherwise error. *)
    method match_module_parts
        ~is_value ~resolve_full module_tree prev_parts rest_parts prev_is_module on_export =
      let open Ast.Identifier in
      let open Module_tree in
      match rest_parts with
      | [] -> failwith "There must be at least two parts in a scoped identifier"
      | ({ name; loc; _ } as part) :: rest_parts ->
        (* Only return an export node if there actually is a value/type exported *)
        let find_name ~is_value name module_tree =
          match SMap.find_opt name module_tree with
          | Some (Export { value = None; _ }) when is_value -> None
          | Some (Export { ty = None; _ }) when not is_value -> None
          | result -> result
        in
        (match (find_name ~is_value name module_tree, rest_parts) with
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
        | (Some (Export { value; ty = _ }), rest_parts)
          when is_value && ((not resolve_full) || rest_parts = []) ->
          (* Values may have additional name parts, as these will be field accesses *)
          let (_, { Ast.Identifier.loc = decl_loc; _ }) = Option.get value in
          this#add_value_use decl_loc loc;
          on_export prev_parts part rest_parts
        | (Some (Export _), _) when is_value ->
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
          this#add_error loc (ReferenceChildOfExport (name, prev_parts_names));
          None
        | (Some (Export { value = _; ty }), []) ->
          (* Types are only fully resolved if all name parts have been matched *)
          let (_, { Ast.Identifier.loc = decl_loc; _ }) = Option.get ty in
          this#add_type_use decl_loc loc;
          on_export prev_parts part rest_parts
        | (Some (Export { value = _; ty }), next_part :: _) ->
          (* Type was fully resolved, but there are still name parts to resolve *)
          let full_loc = Loc.between (List.hd prev_parts).loc next_part.loc in
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
          let (_, ty_name) = Option.get ty in
          this#add_error full_loc (TypeWithAccess (prev_parts_names @ [ty_name.name]));
          None
        | (Some (Empty (_, module_tree)), rest_parts) ->
          this#match_module_parts
            ~is_value
            ~resolve_full
            module_tree
            (prev_parts @ [part])
            rest_parts
            false
            on_export
        | (Some (Module (_, module_tree)), rest_parts) ->
          this#match_module_parts
            ~is_value
            ~resolve_full
            module_tree
            (prev_parts @ [part])
            rest_parts
            true
            on_export)

    method match_module_parts_value module_tree prev_parts rest_parts expr =
      this#match_module_parts
        ~is_value:true
        ~resolve_full:false
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
            | Ast.Expression.NamedAccess ({ target; _ } as access) ->
              if depth = 0 then
                scoped_id
              else
                Ast.Expression.NamedAccess
                  { access with target = insert_scoped_id target (depth - 1) scoped_id }
            | _ -> failwith "Must be nested access expression"
          in
          Some (insert_scoped_id expr (List.length rest_parts) scoped_id))

    method match_module_parts_pattern module_tree first_part rest_parts =
      ignore
        (this#match_module_parts
           ~is_value:true
           ~resolve_full:true
           module_tree
           [first_part]
           rest_parts
           false
           (fun _ _ _ -> None))

    method match_module_parts_type module_tree first_part rest_parts =
      ignore
        (this#match_module_parts
           ~is_value:false
           ~resolve_full:true
           module_tree
           [first_part]
           rest_parts
           false
           (fun _ _ _ -> None))

    method resolve_value_id_use id =
      let { Identifier.loc; name; _ } = id in
      match this#lookup_value_in_scope name scopes with
      | None -> this#add_error loc (UnresolvedName (name, true))
      | Some decl_loc ->
        let declaration = (LocMap.find decl_loc value_bindings).declaration in
        (match declaration with
        | ModuleDecl _ -> this#add_error loc (ModuleInvalidPosition ([name], true))
        | Decl _ -> this#add_value_use decl_loc loc)

    method! expression expr =
      let open Ast.Expression in
      match expr with
      | Identifier { loc; name = "_" } ->
        this#add_error loc InvalidWildcardIdentifier;
        expr
      | Identifier id ->
        this#resolve_value_id_use id;
        expr
      | NamedAccess { target; name; _ } ->
        (* Gather all potential module parts in order if there is an unbroken chain of accesses
           ending in an id *)
        let rec gather_potential_module_parts expr parts =
          match expr with
          | Identifier id -> Some (id :: parts)
          | NamedAccess { target; name; _ } -> gather_potential_module_parts target (name :: parts)
          | _ -> None
        in
        let parts = gather_potential_module_parts target [name] in
        (match parts with
        | None -> super#expression expr
        | Some parts ->
          let open Ast.Identifier in
          let (first_part, rest_parts) = List_utils.split_first parts in
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
            let declaration = (LocMap.find decl_loc value_bindings).declaration in
            (match declaration with
            | ModuleDecl module_tree ->
              (match this#match_module_parts_value module_tree [first_part] rest_parts expr with
              | None -> expr
              | Some resolved_ast -> resolved_ast)
            | Decl _ -> expr)))
      | _ -> super#expression expr

    (* If field shorthand is used, field name is also a variable that must be resolved *)
    method! record_expression_field field =
      let open Expression.Record.Field in
      let { loc = _; name; value } = field in
      (match value with
      | None -> this#resolve_value_id_use name
      | Some _ -> ());
      super#record_expression_field field

    method! type_ ty =
      let open Ast.Type in
      (match ty with
      | Identifier { name = { Ast.ScopedIdentifier.name; scopes = scope_ids; _ }; _ } ->
        let open Ast.Identifier in
        let all_parts = scope_ids @ [name] in
        let (first_part, rest_parts) = List_utils.split_first all_parts in
        let match_module_parts module_tree =
          match rest_parts with
          | [] ->
            let { Ast.Identifier.loc; name } = first_part in
            this#add_error loc (ModuleInvalidPosition ([name], false))
          | _ :: _ -> this#match_module_parts_type module_tree first_part rest_parts
        in
        (match this#lookup_type_in_scope first_part.name scopes with
        | None ->
          (match SMap.find_opt first_part.name module_tree with
          | None ->
            (* Error if first part of scoped id cannot be resolved *)
            this#add_error first_part.loc (UnresolvedName (first_part.name, false))
          | Some (Export _) -> failwith "Exports cannot appear at top level of module tree"
          | Some (Empty (_, module_tree) | Module (_, module_tree)) ->
            match_module_parts module_tree)
        | Some decl_loc ->
          this#add_type_use decl_loc first_part.loc;
          let declaration = (LocMap.find decl_loc type_bindings).declaration in
          (match declaration with
          | ModuleDecl module_tree -> match_module_parts module_tree
          | Decl _ -> ()))
      | _ -> ());
      super#type_ ty

    method! pattern patt =
      this#visit_pattern ~decl:false ~toplevel:false patt;
      patt

    method resolve_scoped_value_id id =
      let open Ast.ScopedIdentifier in
      let { scopes = scope_ids; name; _ } = id in
      let all_parts = scope_ids @ [name] in
      let (first_part, rest_parts) = List_utils.split_first all_parts in
      let match_module_parts module_tree =
        match rest_parts with
        | [] ->
          let { Ast.Identifier.loc; name } = first_part in
          this#add_error loc (ModuleInvalidPosition ([name], false))
        | _ :: _ -> this#match_module_parts_pattern module_tree first_part rest_parts
      in
      match this#lookup_value_in_scope first_part.name scopes with
      | None ->
        (match SMap.find_opt first_part.name module_tree with
        | None ->
          (* Error if first part of scoped id cannot be resolved *)
          this#add_error first_part.loc (UnresolvedName (first_part.name, true))
        | Some (Export _) -> failwith "Exports cannot appear at top level of module tree"
        | Some (Empty (_, module_tree) | Module (_, module_tree)) -> match_module_parts module_tree)
      | Some decl_loc ->
        this#add_value_use decl_loc first_part.loc;
        let declaration = (LocMap.find decl_loc value_bindings).declaration in
        (match declaration with
        | ModuleDecl module_tree -> match_module_parts module_tree
        | Decl _ -> ())

    method visit_pattern ~decl ~toplevel patt =
      let open Ast.Pattern in
      (* Check for the same name appearing twice in a pattern *)
      let ids = Ast_utils.ids_of_pattern patt in
      if not toplevel then
        ignore
          (List.fold_left
             (fun names { Identifier.loc; name } ->
               if SSet.mem name names then (
                 this#add_error loc (DuplicatePatternNames name);
                 names
               ) else
                 SSet.add name names)
             SSet.empty
             ids);
      (* If this is a use then resolve all ids *)
      if not decl then List.iter this#resolve_value_id_use ids;
      (* Resolve all scoped ids in named tuple and record patterns *)
      let rec resolve_scoped_ids patt =
        match patt with
        | Wildcard _
        | Literal _
        | Identifier _ ->
          ()
        | Tuple { Tuple.name; elements; _ } ->
          Option.iter this#resolve_scoped_value_id name;
          List.iter (fun element -> resolve_scoped_ids element) elements
        | Record { Record.name; fields; _ } ->
          this#resolve_scoped_value_id name;
          List.iter (fun { Record.Field.value; _ } -> resolve_scoped_ids value) fields
      in
      resolve_scoped_ids patt
  end

(* Analyze all bindings in modules, building bindings and rewriting scoped identifiers in AST.
   Return a tuple of the newly resolved ASTs, the complete bindings, and any resolution errors. *)
let analyze ~is_stdlib module_tree modules =
  let results =
    List.map
      (fun (file, mod_) ->
        let bindings_builder = new bindings_builder ~module_tree ~is_stdlib in
        let mod' = bindings_builder#module_ mod_ in
        let (bindings, bindings_errors) = bindings_builder#results () in
        ((file, mod'), bindings, bindings_errors))
      modules
  in
  let (modules', bindings, bindings_errors) = List_utils.split3 results in
  let bindings = List.fold_left Bindings.merge Bindings.empty bindings in
  let errors = List.flatten bindings_errors in
  (modules', bindings, errors)
