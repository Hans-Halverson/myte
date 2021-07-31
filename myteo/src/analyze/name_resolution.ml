open Analyze_error
open Ast
open Basic_collections
open Bindings
open Graph
open Immutable_utils

(* 
 * Name Resolution
 *
 * Resolve all names to their declarations in the program. By the end of name resolution, either
 * the following is true or errors have been generated:
 *
 * - All identifiers (and the final name of scoped identifiers) are resolved to a value, and will
 *   be marked as a value or type use of a declaration.
 * - Named access chains will be converted to scoped identifiers if they are a qualified module
 *   or static method. All remaining named accesses are for field or method accesses which must be
 *   resolved during type checking.
 * - There is at most one non-signature method with any given name in a trait/type and all its
 *   super traits. Additionally, a signature method cannot appear below a concrete method higher
 *   in a trait heirarchy.
 * - If the override keyword is used on a method, there exists exactly one super method to override.
 *   The override keyword can be ignored after name resolution.
 *)

(* Data structures for names resolution *)
type 'a local_decl =
  | Decl of 'a
  | ModuleDecl of Module_tree.t

type scope = {
  local_values: ValueBinding.t local_decl SMap.t;
  local_types: TypeBinding.t local_decl SMap.t;
}

type scopes = scope list

(* Data structures for method name checking *)
module MethodLocation = struct
  type t =
    | SuperTrait of (* Trait name *) string * (* Is signature *) bool (* Is static *) * bool
    | BaseTrait of
        (* Method location on base trait *) Loc.t * (* Is override *) bool (* Is static *) * bool

  let compare l1 l2 =
    match (l1, l2) with
    | (BaseTrait (loc1, _, _), BaseTrait (loc2, _, _)) -> Loc.compare loc1 loc2
    | (BaseTrait _, SuperTrait _) -> -1
    | (SuperTrait _, BaseTrait _) -> 1
    | (SuperTrait (t1, _, _), SuperTrait (t2, _, _)) -> String.compare t1 t2
end

module MethodSet = Set.Make (MethodLocation)
module MethodMMap = MultiMap.Make (String) (MethodLocation)

class bindings_builder ~is_stdlib ~bindings ~module_tree =
  object (this)
    inherit Ast_mapper.mapper as super

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : scopes = []

    (* Map from module loc to the toplevel scope for that module *)
    val mutable toplevel_scopes : scopes LocMap.t = LocMap.empty

    val mutable module_name : string list = []

    (* Whether the resolver is currently in a method *)
    val mutable in_method : bool = false

    (* Set of field names for each record type *)
    val mutable record_fields : SSet.t LocMap.t = LocMap.empty

    (* Set of all trait declaration nodes in program *)
    val mutable traits : Ast.TraitDeclaration.t LocMap.t = LocMap.empty

    (* Graph of dependencies between traits, indexed by trait name loc *)
    val mutable trait_graph : LocGraph.t = LocGraph.mk ()

    method add_error loc err = errors <- (loc, err) :: errors

    method errors () = List.rev errors

    method set_current_module module_ =
      let open Module.Module in
      module_name <- Ast_utils.name_parts_of_scoped_ident module_.name

    method save_toplevel_scope loc = toplevel_scopes <- LocMap.add loc scopes toplevel_scopes

    method restore_toplevel_scope loc = scopes <- LocMap.find loc toplevel_scopes

    (* Run a callback in the parent scope, restoring current scope and returning value when complete *)
    method in_parent_scope f =
      let saved_scope = scopes in
      this#exit_scope ();
      let result = f () in
      scopes <- saved_scope;
      result

    method set_record_fields loc names = record_fields <- LocMap.add loc names record_fields

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

    method add_value_declaration loc name context declaration =
      let binding = ValueBinding.mk ~name ~loc ~declaration ~context ~module_:module_name in
      Bindings.add_value_binding bindings binding;
      binding

    method add_type_declaration loc name declaration =
      let binding = TypeBinding.mk ~name ~loc ~declaration ~module_:module_name in
      Bindings.add_type_binding bindings binding;
      binding

    method is_value_decl decl_loc = LocMap.mem decl_loc bindings.value_bindings

    method is_type_decl decl_loc = LocMap.mem decl_loc bindings.type_bindings

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

    (* Visit all toplevel declarations in a module, creating bindings for each *)
    method add_toplevel_declarations mod_ =
      let open Ast.Module in
      let { module_; toplevels; _ } = mod_ in
      this#set_current_module module_;
      let module_name_prefix =
        String.concat "." (Ast_utils.name_parts_of_scoped_ident module_.name) ^ "."
      in
      (* Gather toplevel type and variable declarations and add them to toplevel scope *)
      let register_stdlib_decl name =
        if is_stdlib then
          let full_name = module_name_prefix ^ name.Identifier.name in
          Std_lib.register_stdlib_decl full_name name.loc
      in
      let register_stdlib_method_decls trait_decl =
        (* Register builtin methods *)
        if is_stdlib then
          let trait_name = trait_decl.Ast.TraitDeclaration.name in
          List.iter
            (fun { Function.name = method_name; _ } ->
              let open Identifier in
              let full_name =
                Printf.sprintf "%s%s.%s" module_name_prefix trait_name.name method_name.name
              in
              Std_lib.register_stdlib_decl full_name method_name.loc)
            trait_decl.methods
      in
      let add_value_binding name declaration =
        ignore
          (this#add_value_declaration name.Identifier.loc name.name ValueBinding.Module declaration)
      in
      let add_type_binding name declaration =
        ignore (this#add_type_declaration name.Identifier.loc name.name declaration)
      in
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.kind; pattern; _ } ->
            let ids = Ast_utils.ids_of_pattern pattern in
            List.iter (fun id -> add_value_binding id (VarDecl (VariableDeclaration.mk kind))) ids
          | FunctionDeclaration { Ast.Function.name; builtin; static; override; body; _ } ->
            register_stdlib_decl name;
            add_value_binding
              name
              (FunDecl
                 (FunctionDeclaration.mk
                    ~name:name.name
                    ~loc:name.loc
                    ~is_builtin:builtin
                    ~is_static:static
                    ~is_override:override
                    ~is_signature:(body = Signature)))
          | TypeDeclaration { Ast.TypeDeclaration.name; decl; _ } ->
            register_stdlib_decl name;
            ( if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else
              match decl with
              | Alias _ -> add_type_binding name (TypeAlias (TypeAliasDeclaration.mk ()))
              | Tuple { name; _ }
              | Record { name; _ } ->
                let type_decl = TypeDeclaration.mk ~name:name.name in
                add_value_binding name (CtorDecl type_decl);
                add_type_binding name (TypeDecl type_decl);
                Std_lib.register_stdlib_type name.loc type_decl.adt_sig
              | Variant variants ->
                let type_decl = TypeDeclaration.mk ~name:name.name in
                add_type_binding name (TypeDecl type_decl);
                Std_lib.register_stdlib_type name.loc type_decl.adt_sig;
                List.iter
                  (fun variant ->
                    let open Ast.TypeDeclaration in
                    match variant with
                    | EnumVariant name
                    | TupleVariant { Tuple.name; _ }
                    | RecordVariant { Record.name; _ } ->
                      add_value_binding name (CtorDecl type_decl))
                  variants
              | Builtin ->
                let type_decl = TypeDeclaration.mk ~name:name.name in
                add_type_binding name (TypeDecl type_decl);
                Std_lib.register_stdlib_type name.loc type_decl.adt_sig );
            (* Check record fields for duplicates and save to compare against methods *)
            (match decl with
            | Record { name = { loc; _ }; fields; _ } ->
              let names =
                List.fold_left
                  (fun names { Ast.TypeDeclaration.Record.Field.name = { loc; name }; _ } ->
                    if SSet.mem name names then (
                      this#add_error loc (DuplicateRecordFieldNames name);
                      names
                    ) else
                      SSet.add name names)
                  SSet.empty
                  fields
              in
              this#set_record_fields loc names
            | _ -> ())
          | TraitDeclaration ({ kind = Methods; name; _ } as trait_decl) ->
            register_stdlib_decl name;
            register_stdlib_method_decls trait_decl;
            add_type_binding name (TraitDecl (TraitDeclaration.mk ~name:name.name ~loc:name.loc))
          | TraitDeclaration ({ kind = Trait; name; _ } as trait_decl) ->
            if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else
              let decl = TraitDeclaration.mk ~name:name.name ~loc:name.loc in
              register_stdlib_decl name;
              Std_lib.register_stdlib_trait name.loc decl.trait_sig;
              register_stdlib_method_decls trait_decl;
              traits <- LocMap.add name.loc trait_decl traits;
              add_type_binding name (TraitDecl decl))
        toplevels

    (* Setup and save the toplevel scope for this module, consisting of all imports and declarations *)
    method setup_toplevel_scope mod_ =
      let open Ast.Module in
      let { loc; module_; toplevels; imports; _ } = mod_ in
      this#set_current_module module_;
      let check_duplicate_toplevel_name name =
        let { Ast.Identifier.loc; name } = name in
        if SMap.mem name (List.hd scopes).local_values || SMap.mem name (List.hd scopes).local_types
        then
          this#add_error loc (DuplicateToplevelNames name)
      in
      let add_imported_value_to_scope name decl_loc =
        this#add_value_to_scope name (Decl (this#get_value_binding decl_loc))
      in
      let add_imported_type_to_scope name decl_loc =
        this#add_type_to_scope name (Decl (this#get_type_binding decl_loc))
      in
      let add_value_decl_to_scope decl_id =
        check_duplicate_toplevel_name decl_id;
        this#add_value_to_scope decl_id.name (Decl (this#get_value_binding decl_id.loc))
      in
      let add_type_decl_to_scope decl_id =
        check_duplicate_toplevel_name decl_id;
        this#add_type_to_scope decl_id.name (Decl (this#get_type_binding decl_id.loc))
      in
      let add_value_and_type_decl_to_scope decl_id =
        check_duplicate_toplevel_name decl_id;
        this#add_value_to_scope decl_id.name (Decl (this#get_value_binding decl_id.loc));
        this#add_type_to_scope decl_id.name (Decl (this#get_type_binding decl_id.loc))
      in
      this#enter_scope ();

      (* Add all implicit imports to toplevel scope *)
      if not is_stdlib then (
        let open Std_lib in
        add_imported_type_to_scope "Bool" (lookup_stdlib_decl_loc std_bool_bool);
        add_imported_type_to_scope "Byte" (lookup_stdlib_decl_loc std_byte_byte);
        add_imported_type_to_scope "Int" (lookup_stdlib_decl_loc std_int_int);
        add_imported_type_to_scope "Long" (lookup_stdlib_decl_loc std_long_long);
        add_imported_type_to_scope "Unit" (lookup_stdlib_decl_loc std_unit_unit);
        add_imported_type_to_scope "String" (lookup_stdlib_decl_loc std_string_string)
      );

      (* Gather imports and add them to toplevel scope *)
      List.iter
        (fun import ->
          let open Import in
          let resolve_import name local_name scopes =
            let open Module_tree in
            let name_parts = scopes @ [name] in
            match lookup name_parts module_tree with
            | LookupResultExport { loc = decl_loc; _ } ->
              check_duplicate_toplevel_name local_name;
              if this#is_value_decl decl_loc then
                add_imported_value_to_scope local_name.name decl_loc;
              if this#is_type_decl decl_loc then add_imported_type_to_scope local_name.name decl_loc
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

      (* Add toplevel declarations to toplevel scope *)
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.pattern; _ } ->
            let ids = Ast_utils.ids_of_pattern pattern in
            List.iter add_value_decl_to_scope ids
          | FunctionDeclaration { Ast.Function.name; _ } -> add_value_decl_to_scope name
          | TypeDeclaration { Ast.TypeDeclaration.name; decl; _ } ->
            (match decl with
            | Builtin
            | Alias _ ->
              add_type_decl_to_scope name
            | Tuple { name; _ }
            | Record { name; _ } ->
              add_value_and_type_decl_to_scope name
            | Variant variants ->
              add_type_decl_to_scope name;
              List.iter
                (fun variant ->
                  let open Ast.TypeDeclaration in
                  match variant with
                  | EnumVariant name
                  | TupleVariant { Tuple.name; _ }
                  | RecordVariant { Record.name; _ } ->
                    add_value_decl_to_scope name)
                variants)
          | TraitDeclaration { kind = Methods; _ } -> ()
          | TraitDeclaration { kind = Trait; name; _ } -> add_type_decl_to_scope name)
        toplevels;
      this#save_toplevel_scope loc;
      this#exit_scope ()

    (* Visit all trait and method blocks, filling in methods and implemented traits *)
    method add_methods_and_implemented mod_ =
      let fill_trait_from_decl trait decl =
        let open Ast.TraitDeclaration in
        let { methods; implemented; _ } = decl in

        (* Fill in methods for trait *)
        let methods =
          List.fold_left
            (fun methods { Function.name = { name; loc }; builtin; static; override; body; _ } ->
              let method_ =
                FunctionDeclaration.mk
                  ~name
                  ~loc
                  ~is_builtin:builtin
                  ~is_static:static
                  ~is_override:override
                  ~is_signature:(body = Function.Signature)
              in
              ignore
                (this#add_value_declaration
                   loc
                   name
                   (ValueBinding.Trait decl.name.name)
                   (FunDecl method_));
              if SMap.mem name methods then
                this#add_error loc (DuplicateMethodNames (name, trait.TraitDeclaration.name, []));
              SMap.add name method_ methods)
            SMap.empty
            methods
        in
        trait.TraitDeclaration.methods <- methods;

        (* Fill in implemented traits *)
        let implemented =
          List.fold_left
            (fun implemented { Type.Identifier.name; _ } ->
              (* Resolve implemented trait name *)
              this#resolve_type_scoped_id name;
              match LocMap.find_opt name.name.loc bindings.type_use_to_decl with
              (* Implemented trait name could not be resolved. Error has already been generated
                 so skip implemented trait. *)
              | None -> implemented
              (* Otherwise fill in implemented trait if name resolves to trait, otherwise error
                 and skip implemented trait. *)
              | Some decl_loc ->
                let binding = this#get_type_binding decl_loc in
                (match binding.declaration with
                | TraitDecl implemented_trait ->
                  let loc = implemented_trait.loc in
                  (* Add edge in trait graph if trait extends a trait in this compilation unit *)
                  if decl.kind = Trait && LocMap.mem loc traits then
                    LocGraph.add_edge ~graph:trait_graph loc trait.loc;
                  LocMap.add name.name.loc implemented_trait implemented
                | _ ->
                  this#add_error name.loc (ExpectedTrait (Ast_utils.string_of_scoped_ident name));
                  implemented))
            LocMap.empty
            implemented
        in
        trait.TraitDeclaration.implemented <- implemented
      in

      let { Module.loc; module_; toplevels; _ } = mod_ in
      this#set_current_module module_;
      this#restore_toplevel_scope loc;
      List.iter
        (fun toplevel ->
          match toplevel with
          | Module.TraitDeclaration ({ kind = Trait; name; _ } as decl) ->
            let binding = this#get_type_binding name.loc in
            let trait = get_trait_decl binding in
            LocGraph.add_node ~graph:trait_graph name.loc;
            fill_trait_from_decl trait decl
          | TraitDeclaration ({ kind = Methods; name = { name; loc }; _ } as decl) ->
            (* Check that method declarations appear in same module as type declaration *)
            (match this#lookup_type_in_scope name scopes with
            | None -> this#add_error loc (UnresolvedName (name, NamePositionType))
            | Some (ModuleDecl _) ->
              this#add_error loc (ModuleInvalidPosition ([name], NamePositionType))
            | Some (Decl binding) ->
              if not (this#is_current_module binding.module_) then
                this#add_error loc (MethodDeclarationsInSameModule (name, module_name))
              else
                this#add_type_use binding.loc loc;
              (* Fill in trait for this method block *)
              (match binding.declaration with
              | TypeDecl type_decl ->
                let binding = this#get_type_binding loc in
                let trait = get_trait_decl binding in
                fill_trait_from_decl trait decl;
                TypeDeclaration.add_trait type_decl trait;
                type_decl.adt_sig.traits <- trait.trait_sig :: type_decl.adt_sig.traits;
                trait.trait_sig.adt_sig <- Some type_decl.adt_sig
              | _ -> failwith "Expected type"))
          | _ -> ())
        toplevels;
      this#exit_scope ()

    (* Resolve all names in module to their declarations *)
    method resolve mod_ =
      let open Ast.Module in
      let { loc; module_; toplevels; _ } = mod_ in
      this#set_current_module module_;
      this#restore_toplevel_scope loc;
      (* Then visit child nodes once toplevel scope is complete *)
      let toplevels' =
        id_map_list
          (fun toplevel ->
            match toplevel with
            | VariableDeclaration decl ->
              id_map (this#visit_variable_declaration ~is_toplevel:true) decl toplevel (fun decl' ->
                  VariableDeclaration decl')
            | FunctionDeclaration decl ->
              id_map
                (this#visit_function_declaration ~is_method:false ~is_nested:false)
                decl
                toplevel
                (fun decl' -> FunctionDeclaration decl')
            | TypeDeclaration
                ( { Ast.TypeDeclaration.name = { Ast.Identifier.name; _ }; type_params; _ } as
                type_decl ) ->
              if type_params <> [] then this#enter_scope ();
              this#visit_type_parameters type_params (TypeName name);
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
        id_map (this#visit_variable_declaration ~is_toplevel:false) decl stmt (fun decl' ->
            VariableDeclaration decl')
      | FunctionDeclaration decl ->
        id_map
          (this#visit_function_declaration ~is_method:false ~is_nested:true)
          decl
          stmt
          (fun decl' -> FunctionDeclaration decl')
      | Block block -> id_map this#block block stmt (fun block' -> Block block')
      | _ -> super#statement stmt

    method! block block =
      this#enter_scope ();
      let block' = super#block block in
      this#exit_scope ();
      block'

    method visit_variable_declaration ~is_toplevel decl =
      let { Ast.Statement.VariableDeclaration.kind; pattern; init; annot; loc = _ } = decl in
      let annot' = id_map_opt this#type_ annot in
      let init' = this#expression init in
      let ids = Ast_utils.ids_of_pattern pattern in
      this#visit_pattern ~is_toplevel ~is_match:false pattern;
      List.iter
        (fun { Ast.Identifier.loc; name; _ } ->
          let context =
            if is_toplevel then
              ValueBinding.Module
            else
              ValueBinding.Function
          in
          let binding =
            this#add_value_declaration loc name context (VarDecl (VariableDeclaration.mk kind))
          in
          this#add_value_to_scope name (Decl binding))
        ids;
      if init == init' && annot == annot' then
        decl
      else
        { decl with annot = annot'; init = init' }

    method visit_type_parameters params source =
      ignore
        ((List.fold_left
            (fun param_names { TypeParameter.name = { Ast.Identifier.loc; name }; bounds; _ } ->
              if SSet.mem name param_names then
                this#add_error loc (DuplicateTypeParameterNames (name, source));
              let binding =
                this#add_type_declaration loc name (TypeParam (TypeParamDeclaration.mk ()))
              in
              this#add_type_to_scope name (Decl binding);
              List.iter (fun bound -> ignore (this#type_ (Identifier bound))) bounds;
              SSet.add name param_names)
            SSet.empty)
           params)

    method visit_function_declaration ~is_nested ~is_method decl =
      let open Ast.Function in
      let {
        loc = full_loc;
        name = { Ast.Identifier.loc; name = func_name };
        params;
        type_params;
        builtin;
        static;
        override;
        body;
        _;
      } =
        decl
      in
      ( if is_nested then
        let binding =
          this#add_value_declaration
            loc
            func_name
            ValueBinding.Function
            (FunDecl
               (FunctionDeclaration.mk
                  ~name:func_name
                  ~loc
                  ~is_builtin:builtin
                  ~is_static:static
                  ~is_override:override
                  ~is_signature:(body = Signature)))
        in
        this#add_value_to_scope func_name (Decl binding) );
      this#enter_scope ();
      this#visit_type_parameters type_params (FunctionName func_name);
      (* Add implicit `this` type to scope within method *)
      ( if is_method && not static then
        let binding =
          this#add_value_declaration
            full_loc
            "this"
            ValueBinding.Function
            (FunParamDecl (FunctionParamDeclaration.mk ()))
        in
        this#add_value_to_scope "this" (Decl binding) );
      let _ =
        List.fold_left
          (fun param_names { Param.name = { Ast.Identifier.loc; name; _ }; _ } ->
            if SSet.mem name param_names then
              this#add_error loc (DuplicateParameterNames (name, func_name));
            let binding =
              this#add_value_declaration
                loc
                name
                ValueBinding.Function
                (FunParamDecl (FunctionParamDeclaration.mk ()))
            in
            this#add_value_to_scope name (Decl binding);
            SSet.add name param_names)
          SSet.empty
          params
      in
      let function_ = super#function_ decl in
      this#exit_scope ();
      function_

    method visit_trait_declaration decl =
      let open Ast.TraitDeclaration in
      let { loc; name = { name; _ }; type_params; implemented; methods; _ } = decl in
      this#enter_scope ();

      (* Add implicit `This` type to scope within trait *)
      let binding = this#add_type_declaration loc "This" (TypeAlias (TypeAliasDeclaration.mk ())) in
      this#add_type_to_scope "This" (Decl binding);

      (* Then add type parameters for trait to scope *)
      this#visit_type_parameters type_params (FunctionName name);

      (* Resolve implemented traits and methods *)
      List.iter
        (fun { Type.Identifier.type_args; _ } ->
          List.iter (fun ty -> ignore (this#type_ ty)) type_args)
        implemented;
      let methods' =
        id_map_list
          (fun ({ Function.static; _ } as method_) ->
            (* Static methods cannot reference trait's type parameters, so resolve in parent scope *)
            if static then
              this#in_parent_scope (fun _ ->
                  this#visit_function_declaration ~is_nested:false ~is_method:false method_)
            else (
              in_method <- true;
              let func = this#visit_function_declaration ~is_nested:false ~is_method:true method_ in
              in_method <- false;
              func
            ))
          methods
      in

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
        List.iter this#resolve_value_id_use ids;
        this#visit_pattern ~is_toplevel:false ~is_match:false pattern;
        super#assignment assign

    method! match_case case =
      let { Match.Case.pattern; _ } = case in
      this#enter_scope ();
      this#visit_pattern ~is_toplevel:false ~is_match:true pattern;
      let result = super#match_case case in
      this#exit_scope ();
      result

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
          | Some (Export { loc = decl_loc; _ })
            when (is_value && not (this#is_value_decl decl_loc))
                 || ((not is_value) && not (this#is_type_decl decl_loc)) ->
            None
          | result -> result
        in
        (* If resolving a value we must check for static methods, which are under a type namespace *)
        let resolved_static_method_opt =
          if is_value then
            match (find_name ~is_value:false name module_tree, rest_parts) with
            | (Some (Export { loc = decl_loc; _ }), meth :: rest) ->
              (match this#get_type_binding decl_loc with
              | { TypeBinding.declaration = TraitDecl trait; _ } ->
                if this#maybe_resolve_static_method part meth rest [trait] TraitTrait then
                  Some (on_export (prev_parts @ [part]) meth rest)
                else
                  Some None
              | { TypeBinding.declaration = TypeDecl type_decl; _ } ->
                if this#maybe_resolve_static_method part meth rest type_decl.traits TraitType then
                  Some (on_export (prev_parts @ [part]) meth rest)
                else
                  Some None
              | _ -> None)
            | _ -> None
          else
            None
        in
        (match resolved_static_method_opt with
        | Some resolved_static_method -> resolved_static_method
        | None ->
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
          | (Some (Export { loc = decl_loc; _ }), rest_parts)
            when is_value && this#is_value_decl decl_loc && ((not resolve_full) || rest_parts = [])
            ->
            (* Values may have additional name parts, as these will be field accesses *)
            this#add_value_use decl_loc loc;
            on_export prev_parts part rest_parts
          | (Some (Export _), _) when is_value ->
            let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
            this#add_error loc (ReferenceChildOfExport (name, prev_parts_names));
            None
          | (Some (Export { loc = decl_loc; _ }), []) ->
            (* Types are only fully resolved if all name parts have been matched *)
            this#add_type_use decl_loc loc;
            on_export prev_parts part rest_parts
          | (Some (Export ty_name), next_part :: _) ->
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
              on_export))

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
      | Super loc ->
        if not in_method then this#add_error loc SuperOutsideMethod;
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
          (* Name may be a static method on a type or trait as long as it has additional parts *)
          (match (this#lookup_type_in_scope first_part.name scopes, rest_parts) with
          | (Some (Decl { TypeBinding.declaration = TraitDecl trait; _ }), meth :: rest) ->
            if this#maybe_resolve_static_method first_part meth rest [trait] TraitTrait then
              ScopedIdentifier
                { loc = Loc.between first_part.loc meth.loc; name = meth; scopes = [first_part] }
            else
              expr
          | (Some (Decl { TypeBinding.declaration = TypeDecl type_decl; _ }), meth :: rest) ->
            if this#maybe_resolve_static_method first_part meth rest type_decl.traits TraitType then
              ScopedIdentifier
                { loc = Loc.between first_part.loc meth.loc; name = meth; scopes = [first_part] }
            else
              expr
          | _ ->
            (match this#lookup_value_in_scope first_part.name scopes with
            (* Name may be a direct declaration *)
            | Some (Decl binding) ->
              this#add_value_use binding.loc first_part.loc;
              expr
            (* Name may be a value qualified by a module *)
            | Some (ModuleDecl module_tree) ->
              (match this#match_module_parts_value module_tree [first_part] rest_parts expr with
              | None -> expr
              | Some resolved_ast -> resolved_ast)
            (* Otherwise name is unresolved *)
            | None ->
              this#add_error first_part.loc (UnresolvedName (first_part.name, NamePositionValue));
              expr)))
      | _ -> super#expression expr

    (* Try to resolve the current `type_part.method_part.rest_parts` chain to a static method given
       a collection of traits. Return whether the chain can be resolved to a static method, and on
       success mark a use. *)
    method maybe_resolve_static_method type_part method_part rest_parts traits trait_or_type =
      (* Check traits to determine if a static method with the given name is present for this trait
         or type, and mark a use if one is found. *)
      let { Identifier.loc = name_loc; name } = method_part in
      let has_method =
        List.fold_left
          (fun is_resolved trait ->
            if is_resolved then
              true
            else
              match SMap.find_opt name trait.TraitDeclaration.methods with
              | Some { FunctionDeclaration.loc; is_static; _ } when is_static ->
                this#add_value_use loc name_loc;
                true
              | _ -> false)
          false
          traits
      in
      if has_method then
        (* Static method cannot have any remaining parts to resolve *)
        if rest_parts = [] then
          true
        else (
          this#add_error
            method_part.loc
            (ReferenceChildOfStaticMethod (method_part.name, type_part.name, trait_or_type));
          false
        )
      else (
        this#add_error
          method_part.loc
          (NoStaticMethod (method_part.name, type_part.name, trait_or_type));
        false
      )

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

    method visit_pattern ~is_toplevel ~is_match patt =
      let open Ast.Pattern in
      let rec visit patt names =
        match patt with
        | Wildcard _
        | Literal _ ->
          names
        | Identifier { scopes = []; name = { loc; name }; _ } ->
          (* If in a match, unscoped identifiers may refer to constructors *)
          let is_ctor_in_match =
            if not is_match then
              false
            else
              match this#lookup_value_in_scope name scopes with
              | Some (Decl ({ declaration = CtorDecl _; _ } as binding)) ->
                this#add_value_use binding.loc loc;
                true
              (* If not a constructor this is a match case variable, so add binding *)
              | _ ->
                let binding =
                  this#add_value_declaration
                    loc
                    name
                    Function
                    (MatchCaseVarDecl (MatchCaseVariableDeclaration.mk ()))
                in
                this#add_value_to_scope name (Decl binding);
                false
          in
          (* Check for the same name appearing twice in a pattern *)
          if (not is_toplevel) && not is_ctor_in_match then
            if SSet.mem name names then (
              this#add_error loc (DuplicatePatternNames name);
              names
            ) else
              SSet.add name names
          else
            names
        | Binding { pattern; name = { loc; name }; _ } ->
          let names = visit pattern names in
          (* If in match, add declaration *)
          let binding =
            this#add_value_declaration
              loc
              name
              Function
              (MatchCaseVarDecl (MatchCaseVariableDeclaration.mk ()))
          in
          this#add_value_to_scope name (Decl binding);
          (* Check for the same name appearing twice in a pattern *)
          if SSet.mem name names then (
            this#add_error loc (DuplicatePatternNames name);
            names
          ) else
            SSet.add name names
        (* Resolve scoped ids in enum, tuple, and record constructor patterns *)
        | NamedWildcard { name; _ } ->
          this#resolve_scoped_constructor_id name;
          names
        | Identifier ({ scopes = _ :: _; _ } as name) ->
          this#resolve_scoped_constructor_id name;
          names
        | Or { left; right; _ } -> visit right (visit left names)
        | Tuple { name; elements; _ } ->
          Option.iter this#resolve_scoped_constructor_id name;
          List.fold_left (fun acc element -> visit element acc) names elements
        | Record { name; fields; _ } ->
          this#resolve_scoped_constructor_id name;
          List.fold_left (fun acc { Record.Field.value; _ } -> visit value acc) names fields
      in
      ignore (visit patt SSet.empty)

    (* Check for trait cycles in the program, returning whether there are no trait cycles in the
       program. If a trait cycle is found, an error is generated. *)
    method order_traits () =
      try
        let ordered_locs = LocGraph.topological_sort ~graph:trait_graph in
        let ordered_traits = List.map (fun loc -> LocMap.find loc traits) ordered_locs in
        Some ordered_traits
      with LocGraph.CycleException loc ->
        let trait = LocMap.find loc traits in
        this#add_error loc (CyclicTrait trait.name.name);
        None

    method check_method_names () =
      let errors = ref [] in
      let add_error loc err = errors := (loc, err) :: !errors in

      (* Collect all method names and *)
      let gather_methods_and_super_traits
          (methods_acc, super_traits_acc) ({ TraitDeclaration.methods; _ } as trait) =
        (* Recursively gather all super trait definitions, deduplicating super traits *)
        let rec gather_super_traits
            super_traits_acc ({ TraitDeclaration.loc; implemented; _ } as super_trait) is_base =
          let super_traits_acc =
            if is_base then
              super_traits_acc
            else
              LocMap.add loc super_trait super_traits_acc
          in
          LocMap.fold
            (fun _ implemented_trait super_traits_acc ->
              gather_super_traits super_traits_acc implemented_trait false)
            implemented
            super_traits_acc
        in
        let methods_acc =
          SMap.fold
            (fun _
                 ( { FunctionDeclaration.loc; is_static; is_override; is_signature; is_builtin; _ }
                 as method_ )
                 methods_acc ->
              if is_static then
                (* Static methods cannot be overridden and must have an implementation *)
                if is_override then (
                  this#add_error loc StaticMethodOverride;
                  methods_acc
                ) else if is_signature && not is_builtin then (
                  this#add_error loc StaticMethodSignature;
                  methods_acc
                ) else
                  LocMap.add loc method_ methods_acc
              else
                LocMap.add loc method_ methods_acc)
            methods
            methods_acc
        in
        let super_traits_acc = gather_super_traits super_traits_acc trait true in
        (methods_acc, super_traits_acc)
      in

      (* Combine all methods names in base trait and its super traits into single data structure *)
      let collect_method_names methods super_traits =
        let names =
          LocMap.fold
            (fun loc { FunctionDeclaration.name; is_override; is_static; _ } names ->
              MethodMMap.add name (MethodLocation.BaseTrait (loc, is_override, is_static)) names)
            methods
            MethodMMap.empty
        in
        let names =
          LocMap.fold
            (fun _ { TraitDeclaration.name = trait_name; methods; _ } names ->
              SMap.fold
                (fun name { FunctionDeclaration.is_signature; is_static; _ } names ->
                  MethodMMap.add
                    name
                    (MethodLocation.SuperTrait (trait_name, is_signature, is_static))
                    names)
                methods
                names)
            super_traits
            names
        in
        names
      in

      (* Find errors for methods. Error on duplicate method names, and override methods that do not
         have a corresponding super trait method. *)
      let check_errors trait_name trait_name_loc names =
        let field_names = LocMap.find_opt trait_name_loc record_fields in
        SMap.iter
          (fun method_name method_locations ->
            (* Collect all method declarations for base trait, non-signature method declarations in
               super traits, whether the method is override in this trait, and whether the method has
               any super trait decls. *)
            let ( base_trait_decls,
                  sig_super_trait_decls,
                  non_sig_super_trait_decls,
                  static_super_trait_decls,
                  override_decl_opt ) =
              MethodSet.fold
                (fun method_location
                     ( base_trait_decls,
                       sig_super_trait_decls,
                       non_sig_super_trait_decls,
                       static_super_trait_decls,
                       override_decl_opt ) ->
                  match method_location with
                  | MethodLocation.BaseTrait (base_trait_decl, is_override, is_static) ->
                    let override_decl_opt =
                      if is_override then
                        Some base_trait_decl
                      else
                        override_decl_opt
                    in
                    ( (base_trait_decl, is_static) :: base_trait_decls,
                      sig_super_trait_decls,
                      non_sig_super_trait_decls,
                      static_super_trait_decls,
                      override_decl_opt )
                  | MethodLocation.SuperTrait (super_trait_decl, is_signature, is_static) ->
                    if is_static then
                      ( base_trait_decls,
                        sig_super_trait_decls,
                        non_sig_super_trait_decls,
                        super_trait_decl :: static_super_trait_decls,
                        override_decl_opt )
                    else if is_signature then
                      ( base_trait_decls,
                        super_trait_decl :: sig_super_trait_decls,
                        non_sig_super_trait_decls,
                        static_super_trait_decls,
                        override_decl_opt )
                    else
                      ( base_trait_decls,
                        sig_super_trait_decls,
                        super_trait_decl :: non_sig_super_trait_decls,
                        static_super_trait_decls,
                        override_decl_opt ))
                method_locations
                ([], [], [], [], None)
            in
            let base_trait_decls = List.rev base_trait_decls in
            let sig_super_trait_decls = List.rev sig_super_trait_decls in
            let non_sig_super_trait_decls = List.rev non_sig_super_trait_decls in
            let _static_super_trait_decls = List.rev static_super_trait_decls in

            let potentially_conflicting_super_trait_decls =
              match override_decl_opt with
              | Some override_decl_loc
                when sig_super_trait_decls = [] && non_sig_super_trait_decls = [] ->
                (* Error if trying to override a nonexistent method *)
                add_error override_decl_loc (OverrideNonexistentMethod method_name);
                []
              | Some override_decl_loc ->
                (* Error if multiple super methods are overridden *)
                (match non_sig_super_trait_decls with
                | super1 :: super2 :: _ ->
                  add_error
                    override_decl_loc
                    (OverrideMultipleMethods (method_name, super1, super2))
                | _ -> ());
                (* If overridden there are no potential conflicts with super traits *)
                []
              | None -> non_sig_super_trait_decls
            in

            (* Error on duplicate method names, ignoring all signatures in super traits *)
            match (base_trait_decls, potentially_conflicting_super_trait_decls) with
            | (_ :: (base_trait_loc, _) :: _, _) ->
              add_error base_trait_loc (DuplicateMethodNames (method_name, trait_name, []))
            | ([(base_trait_loc, _)], super :: _) ->
              add_error base_trait_loc (DuplicateMethodNames (method_name, trait_name, [super]))
            | ([], super1 :: super2 :: _) ->
              add_error
                trait_name_loc
                (DuplicateMethodNames (method_name, trait_name, [super1; super2]))
            | _ ->
              let (static_base_trait_decls, non_static_base_trait_decls) =
                List.partition (fun (_, is_static) -> is_static) base_trait_decls
              in

              (* Error if there is a signature for this method but no override keyword *)
              (match (override_decl_opt, non_static_base_trait_decls, sig_super_trait_decls) with
              | (None, (base_trait_loc, _) :: _, super_trait_name :: _) ->
                add_error base_trait_loc (MissingOverrideKeyword (method_name, super_trait_name))
              | _ -> ());

              (* Error if there is a static method in base and signature in super trait *)
              (match (static_base_trait_decls, sig_super_trait_decls) with
              | ((base_trait_loc, _) :: _, super :: _) ->
                add_error base_trait_loc (DuplicateMethodNames (method_name, trait_name, [super]))
              | _ -> ());

              (* Error if method has same as record field *)
              (match field_names with
              | None -> ()
              | Some field_names ->
                if SSet.mem method_name field_names then (
                  match (base_trait_decls, potentially_conflicting_super_trait_decls) with
                  | ((base_trait_loc, _) :: _, _) ->
                    this#add_error
                      base_trait_loc
                      (DuplicateRecordFieldAndMethodNames (method_name, trait_name, None))
                  | (_, super_trait :: _) ->
                    this#add_error
                      trait_name_loc
                      (DuplicateRecordFieldAndMethodNames (method_name, trait_name, Some super_trait))
                  | _ -> ()
                )))
          names
      in

      LocMap.iter
        (fun _ { TypeBinding.name; loc; declaration; _ } ->
          match declaration with
          | TypeDecl type_decl ->
            let (methods, super_traits) =
              List.fold_left
                gather_methods_and_super_traits
                (LocMap.empty, LocMap.empty)
                type_decl.traits
            in
            let names = collect_method_names methods super_traits in
            check_errors name loc names
          | TraitDecl trait when LocMap.mem loc traits ->
            let (methods, super_traits) =
              gather_methods_and_super_traits (LocMap.empty, LocMap.empty) trait
            in
            let names = collect_method_names methods super_traits in
            check_errors name loc names
          | _ -> ())
        bindings.Bindings.type_bindings;
      List.rev !errors
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
  (* First create bindings for all toplevel declarations in all modules *)
  let bindings_builder = new bindings_builder ~bindings ~module_tree ~is_stdlib in
  List.iter (fun (_, mod_) -> bindings_builder#add_toplevel_declarations mod_) modules;
  (* Then fill in toplevel scopes for all modules *)
  List.iter (fun (_, mod_) -> bindings_builder#setup_toplevel_scope mod_) modules;
  (* Then add methods to all traits and types *)
  List.iter (fun (_, mod_) -> bindings_builder#add_methods_and_implemented mod_) modules;
  (* Then check for trait cycles, only proceeding if there are no trait cycles *)
  let (resolved_modules, ordered_traits, errors) =
    match bindings_builder#order_traits () with
    | Some ordered_traits ->
      (* Then get errors for method declarations in traits and types *)
      let method_field_errors = bindings_builder#check_method_names () in
      (* Then resolve names in each module *)
      let resolved_modules =
        List.map
          (fun (file, mod_) ->
            let resolved_module = bindings_builder#resolve mod_ in
            (file, resolved_module))
          modules
      in
      (resolved_modules, ordered_traits, method_field_errors)
    | None -> (modules, [], [])
  in
  let errors = errors @ bindings_builder#errors () in
  let builtin_errors =
    if is_stdlib then
      assert_stdlib_builtins_found modules
    else
      []
  in
  (resolved_modules, ordered_traits, builtin_errors @ errors)
