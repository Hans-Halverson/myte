open Basic_collections
open Name_resolution
open Type_context
open Analyze_error

let rec build_type ~cx ty =
  let open Ast.Type in
  match ty with
  | Primitive { Primitive.kind; _ } ->
    let open Primitive in
    (match kind with
    | Unit -> Types.Unit
    | Int -> Types.Int
    | String -> Types.String
    | Bool -> Types.Bool)
  | Function { Function.params; return; _ } ->
    Types.Function { params = List.map (build_type ~cx) params; return = build_type ~cx return }
  | Custom { Custom.name = { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; _ }; _ }; _ } ->
    let source_binding = Type_context.get_source_type_binding ~cx loc in
    TVar source_binding.TypeBinding.tvar_id

let visit_type_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | TypeDeclaration
          { Ast.TypeDeclaration.loc; name = { Ast.Identifier.loc = id_loc; name }; ty; _ } ->
        let binding = LocMap.find id_loc cx.bindings.Bindings.type_bindings in
        let tvar_id = binding.TypeBinding.tvar_id in
        let ty = build_type ~cx ty in
        (* Check if the right hand side is a tvar that has been already unified with this type.
           This can only happen for recursive type aliases. *)
        let rep_ty1 = find_union_rep_type ~cx ty in
        let rep_ty2 = find_union_rep_type ~cx (TVar tvar_id) in
        let is_recursive =
          match (rep_ty1, rep_ty2) with
          | (TVar rep_tvar1, TVar rep_tvar2) when rep_tvar1 = rep_tvar2 -> true
          | _ -> false
        in
        if is_recursive || not (Type_context.unify ~cx ty (TVar tvar_id)) then
          Type_context.add_error
            ~cx
            loc
            (RecursiveTypeAlias (name, find_rep_tvar_id ~cx tvar_id, find_rep_type ~cx ty))
      | _ -> ())
    toplevels

let analyze modules bindings =
  let cx = Type_context.mk ~bindings in
  List.iter (fun (_, module_) -> visit_type_declarations ~cx module_) modules;
  List.rev cx.errors
