open Analyze_error
open Ast
open Basic_collections

type declaration =
  | VarDecl
  | FunDecl
  | ImportedExport
  | ImportedModule of Module_tree.module_tree
  | FunParam

module Binding = struct
  type t = {
    name: string;
    declaration: Loc.t * declaration;
    uses: LocSet.t;
  }
end

type t = {
  use_to_binding: Binding.t LocMap.t;
  declaration_to_binding: Binding.t LocMap.t;
}

type scopes = Loc.t SMap.t list

type binding_builder = {
  name: string;
  declaration: Loc.t * declaration;
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

    val mutable bindings : binding_builder LocMap.t = LocMap.empty

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : Loc.t SMap.t list = []

    method add_error loc err = errors <- (loc, err) :: errors

    method add_declaration loc kind name =
      bindings <- LocMap.add loc { name; declaration = (loc, kind); uses = LocSet.empty } bindings;
      match scopes with
      | [] -> failwith "There must always be a scope"
      | scope :: rest -> scopes <- SMap.add name loc scope :: rest

    method add_use declaration use =
      let binding = LocMap.find declaration bindings in
      binding.uses <- LocSet.add use binding.uses

    method enter_scope () = scopes <- SMap.empty :: scopes

    method exit_scope () = scopes <- List.tl scopes

    method lookup_in_scope name scopes =
      match scopes with
      | [] -> None
      | scope :: rest ->
        (match SMap.find_opt name scope with
        | None -> this#lookup_in_scope name rest
        | Some declaration -> Some declaration)

    method results () =
      let bindings =
        LocMap.map (fun { name; declaration; uses } -> { Binding.name; declaration; uses }) bindings
      in
      let errors = List.rev errors in
      (bindings, errors)

    method! module_ mod_ =
      let open Ast.Module in
      let add_name loc kind name =
        if SMap.mem name (List.hd scopes) then this#add_error loc (DuplicateToplevelNames name);
        this#add_declaration loc kind name
      in
      let { toplevels; imports; _ } = mod_ in
      this#enter_scope ();
      (* Gather imports and add them to toplevel scope *)
      List.iter
        (fun import ->
          let open Import in
          let resolve_import name scopes =
            let name_parts = scopes @ [name] in
            match Module_tree.lookup name_parts module_tree with
            | Module_tree.LookupResultExport _export_id -> ImportedExport
            | Module_tree.LookupResultModule (_, module_tree) -> ImportedModule module_tree
            | Module_tree.LookupResultError (loc, error) ->
              this#add_error loc error;
              ImportedExport
          in
          match import with
          | Simple { name = name_id; scopes; _ } ->
            let { Ast.Identifier.loc; name; _ } = name_id in
            (* Add name to toplevel scope *)
            let kind = resolve_import name_id scopes in
            add_name loc kind name
          | Complex { Complex.aliases; scopes; _ } ->
            (* Add local names to toplevel scope *)
            List.iter
              (fun alias ->
                match alias with
                | { Alias.name; alias = Some local_name; _ }
                | { Alias.name = _ as name as local_name; _ } ->
                  let kind = resolve_import name scopes in
                  let { Ast.Identifier.loc; name; _ } = local_name in
                  add_name loc kind name)
              aliases)
        imports;
      (* Gather toplevel declarations add add them to toplevel scope *)
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.pattern; _ } ->
            let id = identifier_in_pattern pattern in
            let { Ast.Identifier.loc; name; _ } = id in
            add_name loc VarDecl name
          | FunctionDeclaration { Ast.Function.name; _ } ->
            let { Ast.Identifier.loc; name; _ } = name in
            add_name loc FunDecl name)
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
                  FunctionDeclaration decl'))
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
      if add then this#add_declaration loc VarDecl name;
      super#variable_declaration decl

    method visit_function_declaration ~add decl =
      let open Ast.Function in
      let { name = { Ast.Identifier.loc; name = func_name; _ }; params; _ } = decl in
      if add then this#add_declaration loc FunDecl func_name;
      this#enter_scope ();
      let _ =
        List.fold_left
          (fun param_names { Param.name = { Ast.Identifier.loc; name; _ }; _ } ->
            if SSet.mem name param_names then
              this#add_error loc (DuplicateParameterNames (name, func_name));
            this#add_declaration loc FunParam name;
            SSet.add name param_names)
          SSet.empty
          params
      in
      let function_ = super#function_ decl in
      this#exit_scope ();
      function_

    (* Match a sequence of module parts against the module tree, returning the same AST with the
       matched access chain replaced with a scoped id if a match exists. Otherwise error. *)
    method match_module_parts module_tree prev_parts rest_parts prev_is_module expr =
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
            this#add_error full_loc (NoExportInModule (name, prev_parts_names))
          else
            this#add_error full_loc (NoModuleWithName (prev_parts_names @ [name]));
          None
        | (Some (Module _), []) ->
          (* Error if resolved to module as modules are not values *)
          let full_loc = Loc.between (List.hd prev_parts).loc loc in
          let prev_parts_names = List.map (fun { name; _ } -> name) prev_parts in
          this#add_error full_loc (ModuleInValuePosition (prev_parts_names @ [name]));
          None
        | (Some (Export _), _) ->
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
          Some (insert_scoped_id expr (List.length rest_parts) scoped_id)
        | (Some (Empty (_, module_tree)), rest_parts) ->
          this#match_module_parts module_tree (prev_parts @ [part]) rest_parts false expr
        | (Some (Module (_, module_tree)), rest_parts) ->
          this#match_module_parts module_tree (prev_parts @ [part]) rest_parts true expr)

    method! expression expr =
      let open Ast.Expression in
      match expr with
      | Identifier { loc; name; _ } ->
        (match this#lookup_in_scope name scopes with
        | None -> this#add_error loc (UnresolvedName name)
        | Some decl_loc ->
          let (_, declaration) = (LocMap.find decl_loc bindings).declaration in
          (match declaration with
          | ImportedModule _ -> this#add_error loc (ModuleInValuePosition [name])
          | _ -> this#add_use decl_loc loc));
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
          (match this#lookup_in_scope first_part.name scopes with
          | None ->
            (match SMap.find_opt first_part.name module_tree with
            | None ->
              (* Error if first part of access chain cannot be resolved *)
              this#add_error first_part.loc (UnresolvedName first_part.name);
              expr
            | Some (Export _) -> failwith "Exports cannot appear at top level of module tree"
            | Some (Empty (_, module_tree) | Module (_, module_tree)) ->
              (* If some portion of the access chain resolves to an export, replace with scoped id *)
              (match this#match_module_parts module_tree [first_part] rest_parts false expr with
              | None -> expr
              | Some resolved_ast -> resolved_ast))
          | Some decl_loc ->
            this#add_use decl_loc first_part.loc;
            let (_, declaration) = (LocMap.find decl_loc bindings).declaration in
            (match declaration with
            | ImportedModule module_tree ->
              (match this#match_module_parts module_tree [first_part] rest_parts false expr with
              | None -> expr
              | Some resolved_ast -> resolved_ast)
            | _ -> expr)))
      | _ -> super#expression expr
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
