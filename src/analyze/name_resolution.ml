module ModuleDef = Module
open Analyze_error
open Ast
open Basic_collections
open Bindings
open Graph

(* 
 * Name Resolution
 *
 * Resolve all names to their declarations in the program. By the end of name resolution, either
 * the following is true or errors have been generated:
 *
 * - All identifiers (and the final name of scoped identifiers) are resolved to a value, and will
 *   be marked as a value or type use of a declaration.
 * - Named access chains will be marked as scoped identifiers if they are a qualified module
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

module MethodLocationCollection = MakeCollection (MethodLocation)

module MethodSet = MethodLocationCollection.Set
module MethodMMap = MultiMap.Make (SMap) (MethodSet)

let implicit_value_imports =
  let open Std_lib in
  [
    ("None", std_option_none);
    ("Some", std_option_some);
    ("Ok", std_result_ok);
    ("Error", std_result_error);
  ]

let implicit_type_imports =
  let open Std_lib in
  [
    ("Bool", std_bool_bool);
    ("Byte", std_byte_byte);
    ("Int", std_int_int);
    ("Long", std_long_long);
    ("Double", std_double_double);
    ("Unit", std_unit_unit);
    ("Never", std_never_never);
    ("String", std_string_string);
    ("Option", std_option_option);
    ("Result", std_result_result);
    ("Vec", std_vec_vec);
    ("Map", std_map_map);
    ("Set", std_set_set);
  ]

let build_implicit_imports ~is_stdlib ~bindings =
  let open Std_lib in
  (* There are no implicit imports in stdlib since implicit imports have not yet been declared *)
  if is_stdlib then
    (SMap.empty, SMap.empty)
  else
    let implicit_value_imports =
      List.fold_left
        (fun acc (name, full_name) ->
          let decl_loc = lookup_stdlib_decl_loc full_name in
          SMap.add name (Decl (get_value_binding bindings decl_loc)) acc)
        SMap.empty
        implicit_value_imports
    in
    let implicit_type_imports =
      List.fold_left
        (fun acc (name, full_name) ->
          let decl_loc = lookup_stdlib_decl_loc full_name in
          SMap.add name (Decl (get_type_binding bindings decl_loc)) acc)
        SMap.empty
        implicit_type_imports
    in
    (implicit_value_imports, implicit_type_imports)

class bindings_builder ~is_stdlib ~pcx =
  let { Program_context.bindings; module_tree; attribute_store; _ } = pcx in
  let (implicit_value_imports, implicit_type_imports) =
    build_implicit_imports ~is_stdlib ~bindings
  in
  object (this)
    inherit Ast_visitor.visitor as super

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : scopes = []

    (* Map from module loc to the toplevel scope for that module *)
    val mutable toplevel_scopes : scopes LocMap.t = LocMap.empty

    (* Stack of current value binding contexts. If there are multiple items in stack then we are
       in nested contexts, e.g. nested functions, and top of stack is current context. *)
    val mutable context_stack : ValueBinding.context list = []

    val mutable current_module : ModuleDef.t = ModuleDef.none

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

    method set_current_module module_ = current_module <- module_

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
      let binding = ValueBinding.mk ~name ~loc ~declaration ~context ~module_:current_module in
      Bindings.add_value_use bindings binding.loc binding;
      binding

    method add_type_declaration loc name declaration =
      let binding = TypeBinding.mk ~name ~loc ~declaration ~module_:current_module in
      Bindings.add_type_use bindings binding.loc binding;
      binding

    method add_this_declaration func_binding =
      let binding =
        ValueBinding.mk
          ~name:"this"
          ~loc:Loc.none
          ~context:(this#get_current_context ())
          ~declaration:(ThisDecl (ThisDeclaration.mk ()))
          ~module_:current_module
      in
      Bindings.add_this_binding bindings binding;
      let func_decl = get_func_decl func_binding in
      func_decl.this_binding_id <- Some binding.id;
      binding

    method add_scope_named_access loc = Bindings.add_scope_named_access bindings loc

    method push_context context = context_stack <- context :: context_stack

    method pop_context () = context_stack <- List.tl context_stack

    method get_current_context () = List.hd context_stack

    method is_value_decl_loc decl_loc = Bindings.is_value_decl_loc bindings decl_loc

    method is_type_decl_loc decl_loc = Bindings.is_type_decl_loc bindings decl_loc

    method get_value_binding decl_loc = get_value_binding bindings decl_loc

    method get_type_binding decl_loc = get_type_binding bindings decl_loc

    method add_value_use binding use_loc = Bindings.add_value_use bindings use_loc binding

    method add_type_use binding use_loc = Bindings.add_type_use bindings use_loc binding

    method is_current_module module_ = ModuleDef.equal module_ current_module

    method enter_scope () =
      scopes <- { local_values = SMap.empty; local_types = SMap.empty } :: scopes

    method exit_scope () = scopes <- List.tl scopes

    method lookup_value_in_scope name scopes =
      match scopes with
      | [] -> SMap.find_opt name implicit_value_imports
      | { local_values; _ } :: rest ->
        (match SMap.find_opt name local_values with
        | None -> this#lookup_value_in_scope name rest
        | Some declaration -> Some declaration)

    method lookup_type_in_scope name scopes =
      match scopes with
      | [] -> SMap.find_opt name implicit_type_imports
      | { local_types; _ } :: rest ->
        (match SMap.find_opt name local_types with
        | None -> this#lookup_type_in_scope name rest
        | Some declaration -> Some declaration)

    method add_general_attributes loc attributes =
      Attributes.add_general_attributes
        ~store:attribute_store
        ~module_:current_module
        loc
        attributes

    method add_function_attributes func_node =
      Attributes.add_function_attributes ~store:attribute_store ~module_:current_module func_node

    (* Visit all toplevel declarations in a module, creating bindings for each *)
    method add_toplevel_declarations mod_ =
      let { Ast.Module.loc; name; toplevels; _ } = mod_ in
      let module_ = ModuleDef.mk ~name:(Ast_utils.name_parts_of_scoped_ident name.name) in
      ModuleDef.set_module_for_module_loc loc module_;
      this#set_current_module module_;

      let open Ast.Module in
      let module_name_prefix =
        String.concat "." (Ast_utils.name_parts_of_scoped_ident name.name) ^ "."
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
          | VariableDeclaration { Ast.Statement.VariableDeclaration.kind; pattern; attributes; _ }
            ->
            (match pattern with
            | Identifier { name; _ } ->
              this#add_general_attributes name.loc attributes;
              add_value_binding name (VarDecl (VariableDeclaration.mk kind))
            | _ -> this#add_error (Ast_utils.pattern_loc pattern) ToplevelVarWithPattern)
          | FunctionDeclaration
              ({ Ast.Function.name; is_public; is_static; is_override; body; _ } as func_node) ->
            register_stdlib_decl name;
            this#add_function_attributes func_node;
            add_value_binding
              name
              (FunDecl
                 (FunctionDeclaration.mk
                    ~name:name.name
                    ~loc:name.loc
                    ~is_public
                    ~is_static
                    ~is_override
                    ~is_signature:(body = Signature)))
          | TypeDeclaration { Ast.TypeDeclaration.name; decl; attributes; is_public; _ } ->
            register_stdlib_decl name;
            this#add_general_attributes name.loc attributes;
            (if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else
              match decl with
              | Alias _ -> add_type_binding name (TypeAlias (TypeAliasDeclaration.mk ()))
              | Tuple { name; _ }
              | Record { name; _ } ->
                let type_decl =
                  TypeDeclaration.mk
                    ~name:name.name
                    ~loc:name.loc
                    ~module_:current_module
                    ~is_public
                in
                add_value_binding name (CtorDecl type_decl);
                add_type_binding name (TypeDecl type_decl);
                Std_lib.register_stdlib_type name.loc type_decl.adt_sig
              | Variant variants ->
                let type_decl =
                  TypeDeclaration.mk
                    ~name:name.name
                    ~loc:name.loc
                    ~module_:current_module
                    ~is_public
                in
                add_type_binding name (TypeDecl type_decl);
                Std_lib.register_stdlib_type name.loc type_decl.adt_sig;
                List.iter
                  (fun variant ->
                    let open Ast.TypeDeclaration in
                    match variant with
                    | EnumVariant name
                    | TupleVariant { Tuple.name; _ }
                    | RecordVariant { Record.name; _ } ->
                      register_stdlib_decl name;
                      add_value_binding name (CtorDecl type_decl))
                  variants
              | None ->
                let type_decl =
                  TypeDeclaration.mk
                    ~name:name.name
                    ~loc:name.loc
                    ~module_:current_module
                    ~is_public
                in
                add_type_binding name (TypeDecl type_decl);
                Std_lib.register_stdlib_type name.loc type_decl.adt_sig);
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
          | TraitDeclaration ({ kind = Methods; name; attributes; is_public; _ } as trait_decl) ->
            register_stdlib_decl name;
            register_stdlib_method_decls trait_decl;
            this#add_general_attributes name.loc attributes;
            add_type_binding
              name
              (TraitDecl
                 (TraitDeclaration.mk
                    ~name:name.name
                    ~loc:name.loc
                    ~module_:current_module
                    ~is_public))
          | TraitDeclaration ({ kind = Trait; name; attributes; is_public; _ } as trait_decl) ->
            if name.name = "_" then
              this#add_error name.loc InvalidWildcardIdentifier
            else
              let decl =
                TraitDeclaration.mk ~name:name.name ~loc:name.loc ~module_:current_module ~is_public
              in
              register_stdlib_decl name;
              this#add_general_attributes name.loc attributes;
              Std_lib.register_stdlib_trait name.loc decl.trait_sig;
              register_stdlib_method_decls trait_decl;
              traits <- LocMap.add name.loc trait_decl traits;
              add_type_binding name (TraitDecl decl))
        toplevels

    (* Setup and save the toplevel scope for this module, consisting of all imports and declarations *)
    method setup_toplevel_scope mod_ =
      let open Ast.Module in
      let { loc; toplevels; imports; _ } = mod_ in
      this#set_current_module (ModuleDef.get_module_for_module_loc loc);
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

      (* Gather imports and add them to toplevel scope *)
      List.iter
        (fun import ->
          let open Import in
          let resolve_import name local_name scopes =
            let open Module_tree in
            let name_parts = scopes @ [name] in
            match lookup name_parts module_tree with
            | LookupResultDecl { name = { loc = decl_loc; _ }; is_public } ->
              if not is_public then this#add_error name.loc (ImportPrivateDecl name.name);

              check_duplicate_toplevel_name local_name;
              if this#is_value_decl_loc decl_loc then
                add_imported_value_to_scope local_name.name decl_loc;
              if this#is_type_decl_loc decl_loc then
                add_imported_type_to_scope local_name.name decl_loc
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
            (match pattern with
            | Identifier { name; _ } -> add_value_decl_to_scope name
            | _ -> ())
          | FunctionDeclaration { Ast.Function.name; _ } -> add_value_decl_to_scope name
          | TypeDeclaration { Ast.TypeDeclaration.name; decl; _ } ->
            (match decl with
            | None
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
            (fun methods
                 ({ Function.name = { name; loc }; is_public; is_static; is_override; body; _ } as
                 func_node) ->
              this#add_function_attributes func_node;
              let method_ =
                FunctionDeclaration.mk
                  ~name
                  ~loc
                  ~is_public
                  ~is_static
                  ~is_override
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
              match LocMap.find_opt name.name.loc bindings.type_use_to_binding with
              (* Implemented trait name could not be resolved. Error has already been generated
                 so skip implemented trait. *)
              | None -> implemented
              (* Otherwise fill in implemented trait if name resolves to trait, otherwise error
                 and skip implemented trait. *)
              | Some binding ->
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

      let check_methods_visibility trait trait_is_public =
        List.iter
          (fun { Ast.Function.name = { loc; _ }; is_public; _ } ->
            if is_public && not trait_is_public then
              let trait_type =
                if trait.Ast.TraitDeclaration.kind == Methods then
                  TraitType
                else
                  TraitTrait
              in
              this#add_error loc (PublicMethodInPrivateTrait trait_type))
          trait.methods
      in

      let { Module.loc; toplevels; _ } = mod_ in
      this#set_current_module (ModuleDef.get_module_for_module_loc loc);
      this#restore_toplevel_scope loc;
      List.iter
        (fun toplevel ->
          match toplevel with
          | Module.TraitDeclaration ({ kind = Trait; name; is_public; _ } as decl) ->
            let binding = this#get_type_binding name.loc in
            let trait = get_trait_decl binding in
            LocGraph.add_node ~graph:trait_graph name.loc;
            check_methods_visibility decl is_public;
            fill_trait_from_decl trait decl
          | TraitDeclaration ({ kind = Methods; name = { name; loc }; _ } as decl) ->
            (* Check that method declarations appear in same module as type declaration *)
            (match this#lookup_type_in_scope name scopes with
            | None -> this#add_error loc (UnresolvedName (name, NamePositionType))
            | Some (ModuleDecl _) ->
              this#add_error loc (ModuleInvalidPosition ([name], NamePositionType))
            | Some (Decl type_binding) ->
              if not (this#is_current_module type_binding.module_) then
                this#add_error loc (MethodDeclarationsInSameModule (name, current_module.name));
              (match type_binding.declaration with
              | TypeDecl type_decl ->
                let trait_binding = this#get_type_binding loc in
                let trait_decl = get_trait_decl trait_binding in
                check_methods_visibility decl type_decl.is_public;
                (* Fill in trait for this method block *)
                fill_trait_from_decl trait_decl decl;
                TypeDeclaration.add_trait type_decl trait_decl;
                type_decl.adt_sig.traits <- trait_decl.trait_sig :: type_decl.adt_sig.traits;
                trait_decl.trait_sig.adt_sig <- Some type_decl.adt_sig
              | _ -> this#add_error loc (UnresolvedName (name, NamePositionType))))
          | _ -> ())
        toplevels;
      this#exit_scope ()

    (* Resolve all names in module to their declarations *)
    method resolve mod_ =
      let open Ast.Module in
      let { loc; toplevels; _ } = mod_ in
      this#set_current_module (ModuleDef.get_module_for_module_loc loc);
      this#restore_toplevel_scope loc;
      (* Then visit child nodes once toplevel scope is complete *)
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration decl -> this#visit_variable_declaration ~is_toplevel:true decl
          | FunctionDeclaration decl ->
            this#visit_function_declaration ~is_method:false ~is_nested:false decl
          | TypeDeclaration
              ({ Ast.TypeDeclaration.name = { Ast.Identifier.name; _ }; type_params; _ } as
              type_decl) ->
            if type_params <> [] then this#enter_scope ();
            this#visit_type_parameters type_params (TypeName name);
            this#type_declaration type_decl;
            if type_params <> [] then this#exit_scope ()
          | TraitDeclaration decl -> this#visit_trait_declaration decl)
        toplevels;
      this#exit_scope ()

    method! type_declaration decl =
      (* Public fields cannot appear in private types *)
      (match decl.decl with
      | Record { fields; _ } when not decl.is_public ->
        List.iter
          (fun { Ast.TypeDeclaration.Record.Field.name; is_public; _ } ->
            if is_public then this#add_error name.loc PublicFieldInPrivateType)
          fields
      | _ -> ());
      super#type_declaration decl

    method! statement stmt =
      let open Ast.Statement in
      match stmt with
      | VariableDeclaration decl -> this#visit_variable_declaration ~is_toplevel:false decl
      | FunctionDeclaration decl ->
        this#visit_function_declaration ~is_method:false ~is_nested:true decl
      | _ -> super#statement stmt

    method! block block =
      this#enter_scope ();
      let block' = super#block block in
      this#exit_scope ();
      block'

    method visit_variable_declaration ~is_toplevel decl =
      let {
        Ast.Statement.VariableDeclaration.loc = _;
        kind;
        pattern;
        init;
        annot;
        attributes = _;
        is_public = _;
      } =
        decl
      in
      Option.iter this#type_ annot;

      (* Set global init context if this is toplevel *)
      if is_toplevel then (
        this#push_context GlobalInit;
        this#expression init;
        this#pop_context ()
      ) else
        this#expression init;

      (* Toplevel variable declarations cannot contain patterns, and are added to module scope *)
      if is_toplevel then
        match pattern with
        | Identifier { name = { loc; name }; _ } ->
          let binding = this#get_value_binding loc in
          this#add_value_to_scope name (Decl binding)
        | _ -> ()
      else
        this#visit_pattern
          ~is_match:false
          ~mk_decl:(Some (fun _ -> VarDecl (VariableDeclaration.mk kind)))
          pattern

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

    method visit_function_params func_name param_ids =
      ignore
        (List.fold_left
           (fun param_names { Ast.Identifier.loc; name; _ } ->
             if SSet.mem name param_names then
               this#add_error loc (DuplicateParameterNames (name, func_name));
             let binding =
               this#add_value_declaration
                 loc
                 name
                 (this#get_current_context ())
                 (FunParamDecl (FunctionParamDeclaration.mk ()))
             in
             this#add_value_to_scope name (Decl binding);
             SSet.add name param_names)
           SSet.empty
           param_ids)

    method visit_function_declaration ~is_nested ~is_method decl =
      let open Ast.Function in
      let {
        name = { Ast.Identifier.loc; name = func_name };
        params;
        type_params;
        is_static;
        is_override;
        body;
        _;
      } =
        decl
      in
      let func_binding =
        if is_nested then (
          let binding =
            this#add_value_declaration
              loc
              func_name
              (this#get_current_context ())
              (FunDecl
                 (FunctionDeclaration.mk
                    ~name:func_name
                    ~loc
                    ~is_public:false
                    ~is_static
                    ~is_override
                    ~is_signature:(body = Signature)))
          in
          this#add_value_to_scope func_name (Decl binding);
          binding
        ) else
          this#get_value_binding loc
      in
      this#enter_scope ();
      this#push_context (Function (loc, false));
      this#visit_type_parameters type_params (FunctionName func_name);
      (* Add implicit `this` type to scope within method *)
      if is_method && (not is_static) && body <> Signature then (
        let binding = this#add_this_declaration func_binding in
        this#add_value_to_scope "this" (Decl binding);
        this#add_value_to_scope "super" (Decl binding)
      );
      let param_ids = List.map (fun { Param.name; _ } -> name) params in
      this#visit_function_params (Some func_name) param_ids;
      let function_ = super#function_ decl in
      this#pop_context ();
      this#exit_scope ();
      function_

    method! anonymous_function func =
      let open Expression.AnonymousFunction in
      let { loc; params; _ } = func in
      (* Anonymous function body is in new scope that includes function parameters *)
      this#enter_scope ();
      this#push_context (Function (loc, true));
      let param_ids = List.map (fun { Param.name; _ } -> name) params in
      this#visit_function_params None param_ids;
      let func = super#anonymous_function func in
      this#pop_context ();
      this#exit_scope ();
      func

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
      List.iter
        (fun ({ Function.is_static; _ } as method_) ->
          (* Static methods cannot reference trait's type parameters, so resolve in parent scope *)
          if is_static then
            this#in_parent_scope (fun _ ->
                this#visit_function_declaration ~is_nested:false ~is_method:false method_)
          else (
            in_method <- true;
            this#visit_function_declaration ~is_nested:false ~is_method:true method_;
            in_method <- false
          ))
        methods;

      this#exit_scope ()

    method! assignment assign =
      let open Statement.Assignment in
      match assign.lvalue with
      (* LValue expression are resolved like normal expressions *)
      | Expression _ -> super#assignment assign
      (* Resolve every identifier in an LValue pattern to a declaration *)
      | Pattern pattern ->
        let ids = Ast_utils.ids_of_pattern pattern in
        List.iter this#resolve_value_id_use ids;
        this#visit_pattern ~is_match:false ~mk_decl:None pattern;
        super#assignment assign

    (* For loops open a new scope for pattern bindings *)
    method! for_ for_ =
      let open Statement.For in
      let { loc = _; pattern; annot; iterator; body } = for_ in
      (* Type and iterator must be resolved before bindings are added. Bindings are only added for
         body of for loop. *)
      Option.iter this#type_ annot;
      this#expression iterator;
      this#enter_scope ();
      this#visit_pattern
        ~is_match:false
        ~mk_decl:(Some (fun _ -> VarDecl (VariableDeclaration.mk Immutable)))
        pattern;
      this#block body;
      this#exit_scope ()

    method! if_ if_ =
      match if_.test with
      | Expression _ -> super#if_ if_
      | Match match_test ->
        this#visit_match_test_and_block match_test if_.conseq;
        this#if_altern if_.altern

    method! while_ while_ =
      match while_.test with
      | Expression _ -> super#while_ while_
      | Match match_test -> this#visit_match_test_and_block match_test while_.body

    method visit_match_test_and_block match_test block =
      (* Match tests introduces a single case of bindings scoped to a single block *)
      let { Test.Match.loc = _; expr; pattern; guard } = match_test in
      this#expression expr;
      this#enter_scope ();
      this#visit_pattern
        ~is_match:true
        ~mk_decl:(Some (fun _ -> MatchCaseVarDecl (MatchCaseVariableDeclaration.mk ())))
        pattern;
      Option.iter this#expression guard;
      this#block block;
      this#exit_scope ()

    method! match_case case =
      let { Match.Case.pattern; _ } = case in
      this#enter_scope ();
      this#visit_pattern
        ~is_match:true
        ~mk_decl:(Some (fun _ -> MatchCaseVarDecl (MatchCaseVariableDeclaration.mk ())))
        pattern;
      let result = super#match_case case in
      this#exit_scope ();
      result

    (* Match a sequence of module parts against the module tree, marked a scoped acccess chain
       if a match exists. Otherwise error. *)
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
          | Some (Decl { name = { loc = decl_loc; _ }; _ })
            when (is_value && not (this#is_value_decl_loc decl_loc))
                 || ((not is_value) && not (this#is_type_decl_loc decl_loc)) ->
            None
          | result -> result
        in
        let error_if_private is_public =
          let cannot_access = not is_public in
          if not is_public then this#add_error part.loc (AccessPrivateDecl part.name);
          cannot_access
        in
        (* If resolving a value we must check for static methods, which are under a type namespace *)
        let is_static_method =
          if is_value then
            match (find_name ~is_value:false name module_tree, rest_parts) with
            | (Some (Decl { name = { loc = decl_loc; _ }; is_public }), meth :: rest) ->
              (match this#get_type_binding decl_loc with
              | { TypeBinding.declaration = TraitDecl _ | TypeDecl _; _ } as target_binding ->
                if not (error_if_private is_public) then (
                  if this#maybe_resolve_static_method part meth rest target_binding then
                    on_export rest;
                  true
                ) else
                  true
              | _ -> false)
            | _ -> false
          else
            false
        in
        if not is_static_method then (
          (* First check if the name is a private decl *)
          let lookup_result = find_name ~is_value name module_tree in
          (match lookup_result with
          | Some (Decl { is_public; _ }) -> ignore (error_if_private is_public)
          | _ -> ());

          match (lookup_result, rest_parts) with
          | (None, _)
          | (Some (Empty _), []) ->
            (* Error on no match - but check if parent module exists for better error message *)
            let full_loc = Loc.between (List.hd prev_parts).loc loc in
            let prev_parts_names = List.map (fun { Ast.Identifier.name; _ } -> name) prev_parts in
            if prev_is_module then
              this#add_error full_loc (NoDeclInModule (name, prev_parts_names, is_value))
            else
              this#add_error full_loc (NoModuleWithName (prev_parts_names @ [name], is_value))
          | (Some (Module _), []) ->
            (* Error if resolved to module as modules are not types or values *)
            let full_loc = Loc.between (List.hd prev_parts).loc loc in
            let prev_parts_names = List.map (fun { Ast.Identifier.name; _ } -> name) prev_parts in
            let position =
              if is_value then
                NamePositionValue
              else
                NamePositionType
            in
            this#add_error full_loc (ModuleInvalidPosition (prev_parts_names @ [name], position))
          | (Some (Decl { name = { loc = decl_loc; _ }; _ }), rest_parts)
            when is_value
                 && this#is_value_decl_loc decl_loc
                 && ((not resolve_full) || rest_parts = []) ->
            (* Values may have additional name parts, as these will be field accesses *)
            let binding = this#get_value_binding decl_loc in
            this#add_value_use binding loc;
            on_export rest_parts
          | (Some (Decl _), _) when is_value ->
            let prev_parts_names = List.map (fun { Ast.Identifier.name; _ } -> name) prev_parts in
            this#add_error loc (ReferenceChildOfDecl (name, prev_parts_names))
          | (Some (Decl { name = { loc = decl_loc; _ }; _ }), []) ->
            (* Types are only fully resolved if all name parts have been matched *)
            let binding = this#get_type_binding decl_loc in
            this#add_type_use binding loc;
            on_export rest_parts
          | (Some (Decl { name = ty_name; _ }), next_part :: _) ->
            (* Type was fully resolved, but there are still name parts to resolve *)
            let full_loc = Loc.between (List.hd prev_parts).loc next_part.loc in
            let prev_parts_names = List.map (fun { Ast.Identifier.name; _ } -> name) prev_parts in
            this#add_error full_loc (TypeWithAccess (prev_parts_names @ [ty_name.name]))
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
              on_export
        )

    method match_module_parts_value module_tree prev_parts rest_parts expr =
      this#match_module_parts
        ~is_value:true
        ~resolve_full:false
        module_tree
        prev_parts
        rest_parts
        false
        (fun rest_parts ->
          (* Resolved to export - mark nested accesses as scoped identifier *)
          (* Make sure new scoped id only replaces accesses that were matched, using number of
             unmatched accesses to know how how many to preserve. *)
          let rec mark_scoped_id expr depth =
            match expr with
            | Ast.Expression.NamedAccess { loc; target; _ } ->
              if depth = 0 then
                this#add_scope_named_access loc
              else
                mark_scoped_id target (depth - 1)
            | _ -> failwith "Must be nested access expression"
          in
          ignore (mark_scoped_id expr (List.length rest_parts)))

    method match_module_parts_pattern module_tree first_part rest_parts =
      ignore
        (this#match_module_parts
           ~is_value:true
           ~resolve_full:true
           module_tree
           [first_part]
           rest_parts
           false
           (fun _ -> ()))

    method match_module_parts_type module_tree first_part rest_parts =
      ignore
        (this#match_module_parts
           ~is_value:false
           ~resolve_full:true
           module_tree
           [first_part]
           rest_parts
           false
           (fun _ -> ()))

    method resolve_value_id_use id =
      let { Identifier.loc; name; _ } = id in
      match this#lookup_value_in_scope name scopes with
      | None -> this#add_error loc (UnresolvedName (name, NamePositionValue))
      | Some (ModuleDecl _) ->
        this#add_error loc (ModuleInvalidPosition ([name], NamePositionValue))
      | Some (Decl binding) -> this#add_value_use binding loc

    method! expression expr =
      let open Ast.Expression in
      match expr with
      | Identifier { loc; name = "_" } -> this#add_error loc InvalidWildcardIdentifier
      | Identifier id -> this#resolve_value_id_use id
      | NamedAccess { loc; target; name } ->
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
          | ( Some
                (Decl ({ TypeBinding.declaration = TraitDecl _ | TypeDecl _; _ } as target_binding)),
              meth :: rest ) ->
            if this#maybe_resolve_static_method first_part meth rest target_binding then
              this#add_scope_named_access loc
          | _ ->
            (match this#lookup_value_in_scope first_part.name scopes with
            (* Name may be a direct declaration *)
            | Some (Decl binding) -> this#add_value_use binding first_part.loc
            (* Name may be a value qualified by a module *)
            | Some (ModuleDecl module_tree) ->
              this#match_module_parts_value module_tree [first_part] rest_parts expr
            (* Otherwise name is unresolved *)
            | None ->
              this#add_error first_part.loc (UnresolvedName (first_part.name, NamePositionValue)))))
      | _ -> super#expression expr

    (* Try to resolve the current `type_part.method_part.rest_parts` chain to a static method given
       a collection of traits. Return whether the chain can be resolved to a static method, and on
       success mark a use. *)
    method maybe_resolve_static_method type_part method_part rest_parts target_binding =
      let { Identifier.loc = name_loc; name } = method_part in

      (* Check all methods declarations for type traits *)
      let (traits, trait_or_type) =
        match target_binding.declaration with
        | TraitDecl trait_decl -> ([trait_decl], TraitTrait)
        | TypeDecl type_decl -> (type_decl.traits, TraitType)
        | _ -> failwith "Only trait and type declarations can have static methods"
      in

      (* Check traits to determine if a static method with the given name is present for this trait
         or type, and mark a use if one is found. *)
      let has_method =
        List.fold_left
          (fun is_resolved trait ->
            if is_resolved then
              true
            else
              match SMap.find_opt name trait.TraitDeclaration.methods with
              | Some { FunctionDeclaration.loc; is_static; is_public; _ } when is_static ->
                let binding = this#get_value_binding loc in
                this#add_value_use binding name_loc;

                (* Check that static method is visible in current module *)
                if (not is_public) && not (ModuleDef.equal binding.module_ current_module) then
                  this#add_error method_part.loc (AccessPrivateDecl method_part.name);

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
      | Some (Decl binding) ->
        if rest_parts = [] then
          this#add_type_use binding first_part.loc
        else
          (* Error if there are other parts, as types cannot have accesses *)
          let full_loc = Loc.between first_part.loc (List.hd rest_parts).loc in
          this#add_error full_loc (TypeWithAccess [first_part.name])

    method! type_ ty =
      let open Ast.Type in
      (match ty with
      | Identifier { name; _ }
      | Trait { trait = { name; _ }; _ } ->
        this#resolve_type_scoped_id name
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
      | Some (Decl binding) ->
        if rest_parts = [] then
          this#add_value_use binding first_part.loc
        else
          (* Error if there are other parts, as ctors cannot have accesses *)
          let full_loc = Loc.between first_part.loc (List.hd rest_parts).loc in
          this#add_error full_loc (ConstructorWithAccess [first_part.name])

    method visit_pattern ~is_match ~mk_decl patt =
      let open Ast.Pattern in
      let add_name loc name names = SMap.add name loc names in
      let add_name_error_on_duplicate loc name names =
        (* Check for the same name appearing twice in a pattern *)
        if SMap.mem name names then (
          this#add_error loc (DuplicatePatternNames name);
          names
        ) else
          add_name loc name names
      in

      (* If in right hand side of an or pattern, def should have already been created in left
         hand side so add a use if def exists. *)
      let add_name_in_or_rhs loc name names =
        (match this#lookup_value_in_scope name scopes with
        | Some (Decl binding) -> this#add_value_use binding loc
        | _ -> ());
        add_name loc name names
      in

      let visit_identifier ~may_be_ctor ~in_or_rhs { Identifier.loc; name } names =
        let is_match_ctor =
          if is_match then
            match this#lookup_value_in_scope name scopes with
            | Some (Decl ({ declaration = CtorDecl _; _ } as binding)) when may_be_ctor ->
              this#add_value_use binding loc;
              true
            | _ -> false
          else
            false
        in
        if is_match_ctor then
          names
        else if in_or_rhs then
          add_name_in_or_rhs loc name names
        else (
          (match mk_decl with
          | None -> ()
          | Some mk_decl ->
            let binding =
              this#add_value_declaration loc name (this#get_current_context ()) (mk_decl ())
            in
            this#add_value_to_scope name (Decl binding));
          add_name_error_on_duplicate loc name names
        )
      in

      (* Error on any variables that are not defined on both sides of or pattern. Save error locs
         and add errors at end, to avoid duplicate errors for same variable. *)
      let mismatched_or_errors = ref LocMap.empty in
      let add_mismatched_or_errors names1 names2 =
        SMap.iter
          (fun name loc ->
            if not (SMap.mem name names1) then
              mismatched_or_errors := LocMap.add loc name !mismatched_or_errors)
          names2
      in

      let rec visit ~in_or_rhs patt names =
        match patt with
        | Wildcard _
        | Literal _ ->
          names
        (* Identifiers or binding patterns declare variables *)
        | Identifier { scopes = []; name; _ } ->
          visit_identifier ~may_be_ctor:true ~in_or_rhs name names
        | Binding { pattern; name; _ } ->
          let names = visit ~in_or_rhs pattern names in
          visit_identifier ~may_be_ctor:false ~in_or_rhs name names
        (* Resolve scoped ids in enum, tuple, and record constructor patterns *)
        | NamedWildcard { name; _ } ->
          this#resolve_scoped_constructor_id name;
          names
        | Identifier ({ scopes = _ :: _; _ } as name) ->
          this#resolve_scoped_constructor_id name;
          names
        (* Both sides of or patterns must define the same variables. Declarations are created for
           variables on leftmost side, variables on right sides are uses. *)
        | Or { left; right; _ } ->
          let left_names = visit ~in_or_rhs left names in
          let right_names = visit ~in_or_rhs:true right names in
          add_mismatched_or_errors left_names right_names;
          add_mismatched_or_errors right_names left_names;
          left_names
        | Tuple { name; elements; _ } ->
          Option.iter this#resolve_scoped_constructor_id name;
          List.fold_left (fun acc element -> visit ~in_or_rhs element acc) names elements
        | Record { name; fields; _ } ->
          this#resolve_scoped_constructor_id name;
          List.fold_left
            (fun acc { Record.Field.value; _ } -> visit ~in_or_rhs value acc)
            names
            fields
      in
      ignore (visit ~in_or_rhs:false patt SMap.empty);
      LocMap.iter
        (fun loc name -> this#add_error loc (MismatchedOrPatternName name))
        !mismatched_or_errors

    (* Check for trait cycles in the program, returning whether there are no trait cycles in the
       program. If a trait cycle is found, an error is generated. *)
    method order_traits () =
      try
        let ordered_locs = LocGraph.topological_sort ~graph:trait_graph in
        let ordered_traits = List.map (fun loc -> LocMap.find loc traits) ordered_locs in
        Some ordered_traits
      with
      | LocGraph.CycleException loc ->
        let trait = LocMap.find loc traits in
        this#add_error loc (CyclicTrait trait.name.name);
        None

    method order_type_aliases (modules : (string * Ast.Module.t) list) =
      let modules = List.map snd modules in
      match Type_alias.order_type_aliases ~bindings modules with
      | Ok ordered_type_aliases -> Some ordered_type_aliases
      | Error { loc; name; _ } ->
        this#add_error loc (CyclicTypeAlias name.name);
        None

    method check_method_names () =
      let errors = ref [] in
      let add_error loc err = errors := (loc, err) :: !errors in

      (* Collect all method names in a trait *)
      let gather_method_names names_acc { TraitDeclaration.methods; implemented; _ } =
        (* Gather names of all methods on the base trait *)
        let names_acc =
          SMap.fold
            (fun name { FunctionDeclaration.loc; is_static; is_override; is_signature; _ } names_acc ->
              let add_base_trait_method_name () =
                MethodMMap.add
                  name
                  (MethodLocation.BaseTrait (loc, is_override, is_static))
                  names_acc
              in
              if is_static then
                (* Static methods cannot be overridden and must have an implementation *)
                if is_override then (
                  this#add_error loc StaticMethodOverride;
                  names_acc
                ) else if is_signature && not (Attributes.is_builtin ~store:attribute_store loc)
                  then (
                  this#add_error loc (InvalidFunctionSignature StaticMethod);
                  names_acc
                ) else
                  add_base_trait_method_name ()
              else
                add_base_trait_method_name ())
            methods
            names_acc
        in

        (* Recursively gather all method names in super traits. For each method only gather the
           super methods that are in the lowest super trait that defines that method. Note that
           there could be duplicate super methods defined in that same trait. *)
        let rec gather_super_traits names_acc already_implemented trait =
          let { TraitDeclaration.name = trait_name; methods; implemented; _ } = trait in
          let (names_acc, new_already_implemented) =
            SMap.fold
              (fun name
                   { FunctionDeclaration.is_signature; is_static; _ }
                   (names_acc, new_already_implemented) ->
                if SSet.mem name already_implemented then
                  (names_acc, new_already_implemented)
                else
                  let names_acc =
                    MethodMMap.add
                      name
                      (MethodLocation.SuperTrait (trait_name, is_signature, is_static))
                      names_acc
                  in
                  let new_already_implemented = SSet.add name new_already_implemented in
                  (names_acc, new_already_implemented))
              methods
              (names_acc, already_implemented)
          in

          LocMap.fold
            (fun _ implemented_trait names_acc ->
              gather_super_traits names_acc new_already_implemented implemented_trait)
            implemented
            names_acc
        in

        let names_acc =
          LocMap.fold
            (fun _ implemented_trait names_acc ->
              gather_super_traits names_acc SSet.empty implemented_trait)
            implemented
            names_acc
        in

        names_acc
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
        (fun use_loc { TypeBinding.name; loc; declaration; _ } ->
          match declaration with
          | _ when not (this#is_type_decl_loc use_loc) -> ()
          | TypeDecl type_decl ->
            let names = List.fold_left gather_method_names MethodMMap.empty type_decl.traits in
            check_errors name loc names
          | TraitDecl trait when LocMap.mem loc traits ->
            let names = gather_method_names MethodMMap.empty trait in
            check_errors name loc names
          | _ -> ())
        bindings.Bindings.type_use_to_binding;
      List.rev !errors
  end

(* Find all private types that are leaked through public declarations. Handle descending into
   type alias bodies when necessary. *)
class leaked_private_types_visitor ~bindings =
  object (this)
    inherit Ast_visitor.visitor as super

    val mutable current_module : ModuleDef.t = ModuleDef.none

    val mutable errors : Analyze_error.error list = []

    (* Type aliases bodies that should be descended into when checking for leaked types *)
    val mutable type_aliases_to_check : Ast.Type.t LocMap.t = LocMap.empty

    (* Name of the root declaration that is being checked *)
    val mutable current_declaration_name : string = ""

    (* Stack of alias uses that caused visitor to descend into alias body. If nonempty, use the
       first added loc when erroring as this is the alias use in root declaration. *)
    val mutable alias_root_loc_stack : Loc.t list = []

    method set_current_module module_ = current_module <- module_

    method set_current_declaration_name name = current_declaration_name <- name

    method errors = errors

    method add_error loc leakee =
      let loc =
        if alias_root_loc_stack == [] then
          loc
        else
          List_utils.last alias_root_loc_stack
      in
      errors <- (loc, PrivateTypeInPublicDeclaration (leakee, current_declaration_name)) :: errors

    method init_type_aliases (modules : (string * Ast.Module.t) list) =
      List.iter
        (fun (_, { Ast.Module.toplevels; _ }) ->
          List.iter
            (fun toplevel ->
              match toplevel with
              (* All private aliases will need to be descended into to check for leaked types. No
                 need to descend into public aliases as they will be checked themselves. *)
              | Ast.Module.TypeDeclaration { decl = Alias type_; name; is_public; _ }
                when not is_public ->
                type_aliases_to_check <- LocMap.add name.loc type_ type_aliases_to_check
              | _ -> ())
            toplevels)
        modules

    method! identifier_type id =
      let id_loc = id.name.name.loc in
      (match LocMap.find_opt id_loc bindings.Bindings.type_use_to_binding with
      | Some { loc; declaration; module_; _ } when ModuleDef.equal module_ current_module ->
        (match declaration with
        | TypeDecl { is_public; _ }
        | TraitDecl { is_public; _ }
          when not is_public ->
          this#add_error id_loc id.name.name.name
        | TypeAlias _ ->
          (* The aliases in same module must be checked to see if substitution would leak types *)
          (match LocMap.find_opt loc type_aliases_to_check with
          | Some type_alias_body ->
            alias_root_loc_stack <- id_loc :: alias_root_loc_stack;
            this#type_ type_alias_body;
            alias_root_loc_stack <- List.tl alias_root_loc_stack
          | _ -> ())
        | _ -> ())
      | _ -> ());
      super#identifier_type id
  end

let check_leaked_private_types ~bindings modules =
  let visitor = new leaked_private_types_visitor ~bindings in
  visitor#init_type_aliases modules;
  let visit_function_declaration { Ast.Function.name; params; return; type_params; is_public; _ } =
    if is_public then (
      visitor#set_current_declaration_name name.name;
      List.iter visitor#type_parameter type_params;
      List.iter visitor#function_param params;
      Option.iter visitor#type_ return
    )
  in
  List.iter
    (fun (_, { Ast.Module.loc; toplevels; _ }) ->
      visitor#set_current_module (ModuleDef.get_module_for_module_loc loc);
      List.iter
        (fun toplevel ->
          match toplevel with
          | Ast.Module.VariableDeclaration { pattern; annot; is_public; _ } when is_public ->
            let id = List.hd (Ast_utils.ids_of_pattern pattern) in
            visitor#set_current_declaration_name id.name;
            Option.iter visitor#type_ annot
          | FunctionDeclaration func_decl when func_decl.is_public ->
            visit_function_declaration func_decl
          | TraitDeclaration { kind; type_params; methods; is_public; _ } ->
            if kind == Trait && is_public then List.iter visitor#type_parameter type_params;
            List.iter visit_function_declaration methods
          | TypeDeclaration { name; type_params; decl; is_public; _ } when is_public ->
            visitor#set_current_declaration_name name.name;
            List.iter visitor#type_parameter type_params;
            (match decl with
            (* Only public fields of record need to be checked *)
            | Record { fields; _ } ->
              List.iter
                (fun field ->
                  if field.Ast.TypeDeclaration.Record.Field.is_public then visitor#type_ field.ty)
                fields
            (* All other decls must be fully checked *)
            | _ -> visitor#type_declaration_declaration decl)
          | _ -> ())
        toplevels)
    modules;

  visitor#errors

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

(* Analyze all bindings in modules, building bindings and marking scoped identifiers in AST.
   Return a tuple of the newly resolved ASTs, the complete bindings, and any resolution errors. *)
let analyze ~pcx ~is_stdlib modules =
  (* First create bindings for all toplevel declarations in all modules *)
  let bindings_builder = new bindings_builder ~pcx ~is_stdlib in
  List.iter (fun (_, mod_) -> bindings_builder#add_toplevel_declarations mod_) modules;
  (* Then fill in toplevel scopes for all modules *)
  List.iter (fun (_, mod_) -> bindings_builder#setup_toplevel_scope mod_) modules;
  (* Then add methods to all traits and types *)
  List.iter (fun (_, mod_) -> bindings_builder#add_methods_and_implemented mod_) modules;
  (* Then check for trait cycles, only proceeding if there are no trait cycles *)
  let (ordered_traits, method_field_errors) =
    match bindings_builder#order_traits () with
    | Some ordered_traits ->
      (* Then get errors for method declarations in traits and types *)
      let method_field_errors = bindings_builder#check_method_names () in
      (* Then resolve names in each module *)
      List.iter (fun (_, mod_) -> bindings_builder#resolve mod_) modules;
      (ordered_traits, method_field_errors)
    | None -> ([], [])
  in
  (* Order type aliases, only proceeding if there are no type alias cycles *)
  let (ordered_type_aliases, leaked_type_errors) =
    match bindings_builder#order_type_aliases modules with
    | Some ordered_type_aliases ->
      let leaked_type_errors = check_leaked_private_types ~bindings:pcx.bindings modules in

      (ordered_type_aliases, leaked_type_errors)
    | None -> ([], [])
  in
  let errors = method_field_errors @ leaked_type_errors @ bindings_builder#errors () in
  let builtin_errors =
    if is_stdlib then
      assert_stdlib_builtins_found modules
    else
      []
  in
  (ordered_traits, ordered_type_aliases, builtin_errors @ errors)
