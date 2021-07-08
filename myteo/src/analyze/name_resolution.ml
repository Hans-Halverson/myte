open Analyze_error
open Ast
open Basic_collections
open Bindings
open Immutable_utils

type 'a local_decl =
  | Decl of 'a
  | ModuleDecl of Module_tree.t

type scope = {
  local_values: ValueBinding.t local_decl SMap.t;
  local_types: TypeBinding.t local_decl SMap.t;
}

type scopes = scope list

let in_value_namespace export_kind =
  let open Module_tree in
  match export_kind with
  | VarDecl _
  | FunDecl _
  | CtorDecl ->
    true
  | TypeAlias _
  | TraitDecl ->
    false
  | TypeDecl is_ctor -> is_ctor

let in_type_namespace export_kind =
  let open Module_tree in
  match export_kind with
  | VarDecl _
  | FunDecl _
  | CtorDecl ->
    false
  | TypeAlias _
  | TraitDecl
  | TypeDecl _ ->
    true

class bindings_builder ~is_stdlib ~bindings ~module_tree =
  let exports = Module_tree.get_all_exports module_tree in
  let _ =
    List.iter
      (fun (kind, { Ast.Identifier.loc; name }, module_) ->
        let (value_decl, type_decl) =
          match kind with
          | Module_tree.VarDecl kind -> (Some (VarDecl (VariableDeclaration.mk kind)), None)
          | FunDecl is_builtin -> (Some (FunDecl (FunctionDeclaration.mk is_builtin)), None)
          | CtorDecl -> (Some (CtorDecl (ConstructorDeclaration.mk ())), None)
          | TypeDecl is_ctor ->
            let value_decl =
              if is_ctor then
                Some (CtorDecl (ConstructorDeclaration.mk ()))
              else
                None
            in
            (value_decl, Some (TypeDecl (TypeDeclaration.mk ())))
          | TypeAlias alias_decl -> (None, Some (TypeAlias alias_decl))
          | TraitDecl -> (None, Some (TraitDecl (TraitDeclaration.mk ())))
        in
        (match value_decl with
        | Some declaration when not (LocMap.mem loc bindings.Bindings.value_bindings) ->
          let binding = ValueBinding.mk ~loc ~name ~declaration ~is_global:true ~module_ in
          Bindings.add_value_binding bindings binding
        | _ -> ());
        match type_decl with
        | Some declaration when not (LocMap.mem loc bindings.Bindings.type_bindings) ->
          let binding = TypeBinding.mk ~loc ~name ~declaration ~module_ in
          Bindings.add_type_binding bindings binding
        | _ -> ())
      exports
  in
  object (this)
    inherit Ast_mapper.mapper as super

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : scopes = []

    val mutable module_name : string list = []

    method add_error loc err = errors <- (loc, err) :: errors

    method errors () = List.rev errors

    method add_value_to_scope name local_decl =
      match scopes with
      | [] -> failwith "There must always be a scope"
      | { local_values; local_types } :: rest ->
        scopes <- { local_values = SMap.add name local_decl local_values; local_types } :: rest

    method add_type_to_scope name local_decl =
      match scopes with
      | [] -> failwith "There must always be a scope"
      | { local_values; local_types } :: rest ->
        scopes <- { local_types = SMap.add name local_decl local_types; local_values } :: rest

    method add_value_declaration loc name is_global mk_decl =
      let binding =
        match LocMap.find_opt loc bindings.value_bindings with
        | None ->
          let declaration = mk_decl () in
          let binding = ValueBinding.mk ~name ~loc ~declaration ~is_global ~module_:module_name in
          Bindings.add_value_binding bindings binding;
          binding
        | Some binding -> binding
      in
      this#add_value_to_scope name (Decl binding)

    method add_type_declaration loc name mk_decl =
      let binding =
        match LocMap.find_opt loc bindings.type_bindings with
        | None ->
          let declaration = mk_decl () in
          let binding = TypeBinding.mk ~name ~loc ~declaration ~module_:module_name in
          Bindings.add_type_binding bindings binding;
          binding
        | Some binding -> binding
      in
      this#add_type_to_scope name (Decl binding)

    method get_value_binding decl_loc = LocMap.find decl_loc bindings.value_bindings

    method get_type_binding decl_loc = LocMap.find decl_loc bindings.type_bindings

    method add_value_use decl_loc use_loc = Bindings.add_value_use bindings use_loc decl_loc

    method add_type_use decl_loc use_loc = Bindings.add_type_use bindings use_loc decl_loc

    method is_current_module name_parts = List.for_all2 ( = ) name_parts module_name

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

    method! module_ mod_ =
      let open Ast.Module in
      let check_duplicate_toplevel_name name =
        let { Ast.Identifier.loc; name } = name in
        if SMap.mem name (List.hd scopes).local_values || SMap.mem name (List.hd scopes).local_types
        then
          this#add_error loc (DuplicateToplevelNames name)
      in
      let add_value_name name mk_decl =
        check_duplicate_toplevel_name name;
        this#add_value_declaration name.loc name.name true mk_decl
      in
      let add_type_name name mk_decl =
        check_duplicate_toplevel_name name;
        this#add_type_declaration name.loc name.name mk_decl
      in
      let add_imported_value name decl_loc =
        this#add_value_to_scope name (Decl (this#get_value_binding decl_loc))
      in
      let add_imported_type name decl_loc =
        this#add_type_to_scope name (Decl (this#get_type_binding decl_loc))
      in
      let { module_; toplevels; imports; _ } = mod_ in
      module_name <- Ast_utils.name_parts_of_scoped_ident module_.name;
      let module_name_prefix = String.concat "." module_name ^ "." in
      this#enter_scope ();
      (* Add all implicit imports to toplevel scope *)
      if not is_stdlib then (
        let open Std_lib in
        add_imported_type "Bool" (lookup_stdlib_decl_loc std_bool_bool);
        add_imported_type "Byte" (lookup_stdlib_decl_loc std_byte_byte);
        add_imported_type "Int" (lookup_stdlib_decl_loc std_int_int);
        add_imported_type "Long" (lookup_stdlib_decl_loc std_long_long);
        add_imported_type "Unit" (lookup_stdlib_decl_loc std_unit_unit);
        add_imported_type "String" (lookup_stdlib_decl_loc std_string_string)
      );
      (* Gather imports and add them to toplevel scope *)
      List.iter
        (fun import ->
          let open Import in
          let resolve_import name local_name scopes =
            let open Module_tree in
            let name_parts = scopes @ [name] in
            match lookup name_parts module_tree with
            | LookupResultExport (kind, { loc = decl_loc; _ }) ->
              check_duplicate_toplevel_name local_name;
              if in_value_namespace kind then add_imported_value local_name.name decl_loc;
              if in_type_namespace kind then add_imported_type local_name.name decl_loc
            | LookupResultModule (_, module_tree) ->
              (* Modules appear in both value and type namespaces *)
              check_duplicate_toplevel_name local_name;
              this#add_value_to_scope local_name.name (ModuleDecl module_tree);
              this#add_type_to_scope local_name.name (ModuleDecl module_tree)
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
      let register_stdlib_decl name =
        if is_stdlib then
          let full_name = module_name_prefix ^ name.Identifier.name in
          Std_lib.register_stdlib_decl full_name name.loc
      in
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.kind; pattern; _ } ->
            let ids = Ast_utils.ids_of_pattern pattern in
            List.iter
              (fun id -> add_value_name id (fun _ -> VarDecl (VariableDeclaration.mk kind)))
              ids
          | FunctionDeclaration { Ast.Function.name; builtin; _ } ->
            register_stdlib_decl name;
            add_value_name name (fun _ -> FunDecl (FunctionDeclaration.mk builtin))
          | TypeDeclaration { Ast.TypeDeclaration.name; decl; _ } ->
            register_stdlib_decl name;
            if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else (
              match decl with
              | Alias _ -> add_type_name name (fun _ -> TypeAlias (TypeAliasDeclaration.mk ()))
              | Tuple { name; _ }
              | Record { name; _ } ->
                add_value_name name (fun _ -> CtorDecl (ConstructorDeclaration.mk ()));
                this#add_type_declaration name.loc name.name (fun _ ->
                    TypeDecl (TypeDeclaration.mk ()))
              | Variant _ -> add_type_name name (fun _ -> TypeDecl (TypeDeclaration.mk ()))
            )
          | TraitDeclaration { kind = Methods; _ } -> ()
          | TraitDeclaration { kind = Trait; name; _ } ->
            register_stdlib_decl name;
            if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else
              add_type_name name (fun _ -> TraitDecl (TraitDeclaration.mk ())))
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
                  ignore (add_value_name name (fun _ -> CtorDecl (ConstructorDeclaration.mk ()))))
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
              id_map (this#visit_function_declaration ~add_decl:false) decl toplevel (fun decl' ->
                  FunctionDeclaration decl')
            | TypeDeclaration
                ( { Ast.TypeDeclaration.name = { Ast.Identifier.name; _ }; type_params; _ } as
                type_decl ) ->
              if type_params <> [] then this#enter_scope ();
              this#add_type_parameter_declarations type_params (TypeName name);
              ignore (this#type_declaration type_decl);
              if type_params <> [] then this#exit_scope ();
              toplevel
            | TraitDeclaration decl ->
              id_map this#visit_trait_declaration decl toplevel (fun decl' ->
                  TraitDeclaration decl'))
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
        id_map (this#visit_function_declaration ~add_decl:true) decl stmt (fun decl' ->
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
              VarDecl (VariableDeclaration.mk kind)))
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
              this#add_type_declaration loc name (fun _ -> TypeParam (TypeParamDeclaration.mk ()));
              SSet.add name param_names)
            SSet.empty)
           params)

    method visit_function_declaration ~add_decl decl =
      let open Ast.Function in
      let { name = { Ast.Identifier.loc; name = func_name }; params; type_params; builtin; _ } =
        decl
      in
      if add_decl then
        this#add_value_declaration loc func_name false (fun _ ->
            FunDecl (FunctionDeclaration.mk builtin));
      this#enter_scope ();
      this#add_type_parameter_declarations type_params (FunctionName func_name);
      let _ =
        List.fold_left
          (fun param_names { Param.name = { Ast.Identifier.loc; name; _ }; _ } ->
            if SSet.mem name param_names then
              this#add_error loc (DuplicateParameterNames (name, func_name));
            this#add_value_declaration loc name false (fun _ ->
                FunParamDecl (FunctionParamDeclaration.mk ()));
            SSet.add name param_names)
          SSet.empty
          params
      in
      let function_ = super#function_ decl in
      this#exit_scope ();
      function_

    method visit_trait_declaration decl =
      let open Ast.TraitDeclaration in
      let { name = { Ast.Identifier.loc; name }; kind; type_params; implemented; methods; _ } =
        decl
      in
      (* Check that method declarations appear in same module as type declaration *)
      (match kind with
      | Methods ->
        (match this#lookup_type_in_scope name scopes with
        | None -> this#add_error loc (UnresolvedName (name, NamePositionType))
        | Some (ModuleDecl _) ->
          this#add_error loc (ModuleInvalidPosition ([name], NamePositionType))
        | Some (Decl binding) ->
          if not (this#is_current_module binding.module_) then
            this#add_error loc (MethodDeclarationsInSameModule (name, module_name))
          else
            this#add_type_use binding.loc loc)
      | Trait -> ());
      (* Resolve names of methods and implemented traits in new scope containing type params *)
      this#enter_scope ();
      this#add_type_parameter_declarations type_params (FunctionName name);
      List.iter
        (fun { ImplementedTrait.name; type_args; _ } ->
          this#resolve_type_scoped_id name;
          List.iter (fun ty -> ignore (this#type_ ty)) type_args)
        implemented;
      let methods' = id_map_list (this#visit_function_declaration ~add_decl:false) methods in
      this#exit_scope ();
      if methods == methods' then
        decl
      else
        { decl with methods = methods' }

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
          | Some (Export (kind, _))
            when (is_value && not (in_value_namespace kind))
                 || ((not is_value) && not (in_type_namespace kind)) ->
            None
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
          let position =
            if is_value then
              NamePositionValue
            else
              NamePositionType
          in
          this#add_error full_loc (ModuleInvalidPosition (prev_parts_names @ [name], position));
          None
        | (Some (Export (kind, { Ast.Identifier.loc = decl_loc; _ })), rest_parts)
          when is_value && in_value_namespace kind && ((not resolve_full) || rest_parts = []) ->
          (* Values may have additional name parts, as these will be field accesses *)
          this#add_value_use decl_loc loc;
          on_export prev_parts part rest_parts
        | (Some (Export _), _) when is_value ->
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
          this#add_error loc (ReferenceChildOfExport (name, prev_parts_names));
          None
        | (Some (Export (_, { Ast.Identifier.loc = decl_loc; _ })), []) ->
          (* Types are only fully resolved if all name parts have been matched *)
          this#add_type_use decl_loc loc;
          on_export prev_parts part rest_parts
        | (Some (Export (_, ty_name)), next_part :: _) ->
          (* Type was fully resolved, but there are still name parts to resolve *)
          let full_loc = Loc.between (List.hd prev_parts).loc next_part.loc in
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
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
      | None -> this#add_error loc (UnresolvedName (name, NamePositionValue))
      | Some (ModuleDecl _) ->
        this#add_error loc (ModuleInvalidPosition ([name], NamePositionValue))
      | Some (Decl binding) -> this#add_value_use binding.loc loc

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
            this#add_error first_part.loc (UnresolvedName (first_part.name, NamePositionValue));
            expr
          | Some (ModuleDecl module_tree) ->
            (match this#match_module_parts_value module_tree [first_part] rest_parts expr with
            | None -> expr
            | Some resolved_ast -> resolved_ast)
          | Some (Decl binding) ->
            this#add_value_use binding.loc first_part.loc;
            expr))
      | _ -> super#expression expr

    (* If field shorthand is used, field name is also a variable that must be resolved *)
    method! record_expression_field field =
      let open Expression.Record.Field in
      let { loc = _; name; value } = field in
      (match value with
      | None -> this#resolve_value_id_use name
      | Some _ -> ());
      super#record_expression_field field

    method resolve_type_scoped_id id =
      let { Ast.ScopedIdentifier.name; scopes = scope_ids; _ } = id in
      let all_parts = scope_ids @ [name] in
      let (first_part, rest_parts) = List_utils.split_first all_parts in
      match this#lookup_type_in_scope first_part.name scopes with
      | None -> this#add_error first_part.loc (UnresolvedName (first_part.name, NamePositionType))
      | Some (ModuleDecl module_tree) ->
        (match rest_parts with
        | [] ->
          let { Ast.Identifier.loc; name } = first_part in
          this#add_error loc (ModuleInvalidPosition ([name], NamePositionType))
        | _ :: _ -> this#match_module_parts_type module_tree first_part rest_parts)
      | Some (Decl binding) -> this#add_type_use binding.loc first_part.loc

    method! type_ ty =
      let open Ast.Type in
      (match ty with
      | Identifier { name; _ } -> this#resolve_type_scoped_id name
      | _ -> ());
      super#type_ ty

    method! pattern patt =
      this#visit_pattern ~decl:false ~toplevel:false patt;
      patt

    method resolve_scoped_constructor_id id =
      let open Ast.ScopedIdentifier in
      let { scopes = scope_ids; name; _ } = id in
      let all_parts = scope_ids @ [name] in
      let (first_part, rest_parts) = List_utils.split_first all_parts in
      match this#lookup_value_in_scope first_part.name scopes with
      | None -> this#add_error first_part.loc (UnresolvedName (first_part.name, NamePositionCtor))
      | Some (ModuleDecl module_tree) ->
        (match rest_parts with
        | [] ->
          let { Ast.Identifier.loc; name } = first_part in
          this#add_error loc (ModuleInvalidPosition ([name], NamePositionCtor))
        | _ :: _ -> this#match_module_parts_pattern module_tree first_part rest_parts)
      | Some (Decl binding) -> this#add_value_use binding.loc first_part.loc

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
          Option.iter this#resolve_scoped_constructor_id name;
          List.iter (fun element -> resolve_scoped_ids element) elements
        | Record { Record.name; fields; _ } ->
          this#resolve_scoped_constructor_id name;
          List.iter (fun { Record.Field.value; _ } -> resolve_scoped_ids value) fields
      in
      resolve_scoped_ids patt
  end

let assert_stdlib_builtins_found mods =
  let open Std_lib in
  SSet.fold
    (fun builtin_name errors ->
      if SMap.mem builtin_name !stdlib_builtin_name_to_decl_loc then
        errors
      else
        let mods = List.map snd mods in
        let loc = Ast_utils.modules_end_loc mods in
        [(loc, BuiltinNotFound builtin_name)])
    all_stdlib_names
    []

(* Analyze all bindings in modules, building bindings and rewriting scoped identifiers in AST.
   Return a tuple of the newly resolved ASTs, the complete bindings, and any resolution errors. *)
let analyze ~is_stdlib bindings module_tree modules =
  let bindings_builder = new bindings_builder ~bindings ~module_tree ~is_stdlib in
  let resolved_modules =
    List.map
      (fun (file, mod_) ->
        let mod' = bindings_builder#module_ mod_ in
        (file, mod'))
      modules
  in
  let errors = bindings_builder#errors () in
  let builtin_errors =
    if is_stdlib then
      assert_stdlib_builtins_found modules
    else
      []
  in
  (resolved_modules, builtin_errors @ errors)
