open Analyze_error
open Ast
open Basic_collections

module Binding = struct
  type t = {
    name: string;
    declaration: Loc.t;
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
  declaration: Loc.t;
  mutable uses: LocSet.t;
}

let identifier_in_pattern pat =
  let open Pattern in
  match pat with
  | Identifier id -> id

class bindings_builder ~module_info =
  object (this)
    inherit [unit, unit] Ast_visitor.visitor as super

    val mutable bindings : binding_builder LocMap.t = LocMap.empty

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    val mutable scopes : Loc.t SMap.t list = []

    method add_error loc err = errors <- (loc, err) :: errors

    method add_declaration loc name =
      bindings <- LocMap.add loc { name; declaration = loc; uses = LocSet.empty } bindings;
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

    method! module_ acc mod_ =
      let open Ast.Module in
      let { toplevels; _ } = mod_ in
      (* Gather toplevel declarations add add them to toplevel scope *)
      this#enter_scope ();
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration { Ast.Statement.VariableDeclaration.pattern; _ } ->
            let id = identifier_in_pattern pattern in
            let { Ast.Identifier.loc; name; _ } = id in
            if SMap.mem name (List.hd scopes) then this#add_error loc (DuplicateToplevelNames name);
            this#add_declaration loc name
          | FunctionDeclaration { Ast.Function.name; _ } ->
            let { Ast.Identifier.loc; name; _ } = name in
            if SMap.mem name (List.hd scopes) then this#add_error loc (DuplicateToplevelNames name);
            this#add_declaration loc name)
        toplevels;
      (* Then visit child nodes once toplevel scope is complete *)
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration decl -> this#visit_variable_declaration acc decl ~add:false
          | FunctionDeclaration decl -> this#visit_function_declaration acc decl ~add:false)
        toplevels;
      this#exit_scope ()

    method! import acc import =
      let open Ast.Module.Import in
      match import with
      | Simple { Ast.ScopedIdentifier.loc; scopes; name } ->

      | _ -> ();
      super#import acc import

    method! statement acc stmt =
      let open Ast.Statement in
      match stmt with
      | VariableDeclaration decl -> this#visit_variable_declaration acc decl ~add:true
      | FunctionDeclaration decl -> this#visit_function_declaration acc decl ~add:true
      | Block block -> this#block acc block
      | _ -> super#statement acc stmt

    method! block acc block =
      this#enter_scope ();
      super#block acc block;
      this#exit_scope ()

    method visit_variable_declaration ~add acc decl =
      let { Ast.Statement.VariableDeclaration.pattern; _ } = decl in
      let id = identifier_in_pattern pattern in
      let { Ast.Identifier.loc; name; _ } = id in
      if add then this#add_declaration loc name;
      super#variable_declaration acc decl

    method visit_function_declaration ~add acc decl =
      let open Ast.Function in
      let { name = { Ast.Identifier.loc; name = func_name; _ }; params; _ } = decl in
      if add then this#add_declaration loc func_name;
      this#enter_scope ();
      let _ =
        List.fold_left
          (fun param_names { Param.name = { Ast.Identifier.loc; name; _ }; _ } ->
            if SSet.mem name param_names then
              this#add_error loc (DuplicateParameterNames (name, func_name));
            this#add_declaration loc name;
            SSet.add name param_names)
          SSet.empty
          params
      in
      super#function_ acc decl;
      this#exit_scope ()

    method! expression acc expr =
      let open Ast.Expression in
      match expr with
      | Identifier { loc; name; _ } ->
        (match this#lookup_in_scope name scopes with
        | None -> this#add_error loc (UnresolvedName name)
        | Some declaration -> this#add_use declaration loc)
      | _ -> super#expression acc expr
  end

let string_of_scoped_id scoped =
  let { Ast.ScopedIdentifier.scopes; name; _ } = scoped in
  let names = List.map (fun { Ast.Identifier.name; _ } -> name) (scopes @ [name]) in
  String.concat "." names

type module_exports = unit Ast.Identifier.t SMap.t

type module_info = module_exports SMap.t

let build_module_info asts =
  let rec gather_toplevel_ids toplevels =
    let open Ast.Module in
    match toplevels with
    | [] -> SMap.empty
    | VariableDeclaration { Ast.Statement.VariableDeclaration.pattern; _ } :: rest ->
      let ({ Ast.Identifier.name; _ } as id) = identifier_in_pattern pattern in
      SMap.add name id (gather_toplevel_ids rest)
    | FunctionDeclaration { Ast.Function.name = { Ast.Identifier.name; _ } as id; _ } :: rest ->
      SMap.add name id (gather_toplevel_ids rest)
  in
  List.fold_left
    (fun (modules, errors) { Ast.Module.loc; name; toplevels; _ } ->
      let module_name = string_of_scoped_id name in
      if SMap.mem module_name modules then
        let error = (loc, DuplicateModuleNames module_name) in
        (modules, error :: errors)
      else
        let toplevels = gather_toplevel_ids toplevels in
        (SMap.add module_name toplevels modules, errors))
    (SMap.empty, [])
    asts

let analyze modules =
  let (_module_info, module_errors) = build_module_info modules in
  let results =
    List.map
      (fun mod_ ->
        let bindings_builder = new bindings_builder in
        bindings_builder#module_ () mod_;
        bindings_builder#results ())
      modules
  in
  let (bindings, bindings_errors) = List.split results in
  let bindings =
    List.fold_left (fun bindings acc -> LocMap.fold LocMap.add bindings acc) LocMap.empty bindings
  in
  let errors = List.concat [module_errors; List.flatten bindings_errors] in
  (bindings, errors)
