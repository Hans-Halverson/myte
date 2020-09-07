open Basic_collections
open Immutable_utils
open Types
module Bindings = Name_resolution.Bindings

type t = {
  bindings: Bindings.t;
  mutable errors: (Loc.t * Analyze_error.t) list;
  mutable union_forest_nodes: union_forest_node IMap.t;
}

and union_forest_node =
  | Rep of {
      ty: Types.t;
      rank: int;
    }
  | Link of Types.tvar_id

let mk ~bindings = { bindings; errors = []; union_forest_nodes = IMap.empty }

let add_error ~cx loc error = cx.errors <- (loc, error) :: cx.errors

let get_source_value_binding ~cx use_loc =
  let open Name_resolution in
  let open Bindings in
  let local_decl_loc = LocMap.find use_loc cx.bindings.value_use_to_decl in
  let local_binding = LocMap.find local_decl_loc cx.bindings.value_bindings in
  match local_binding.ValueBinding.declaration with
  | (_, ImportedValue { Ast.Identifier.loc = source_decl_loc; _ }) ->
    LocMap.find source_decl_loc cx.bindings.value_bindings
  | _ -> local_binding

let get_source_type_binding ~cx use_loc =
  let open Name_resolution in
  let open Bindings in
  let local_decl_loc = LocMap.find use_loc cx.bindings.type_use_to_decl in
  let local_binding = LocMap.find local_decl_loc cx.bindings.type_bindings in
  match local_binding.TypeBinding.declaration with
  | (_, ImportedType { Ast.Identifier.loc = source_decl_loc; _ }) ->
    LocMap.find source_decl_loc cx.bindings.type_bindings
  | _ -> local_binding

let set_union_find_node ~cx tvar_id node =
  cx.union_forest_nodes <- IMap.add tvar_id node cx.union_forest_nodes

let add_tvar ~cx tvar_id =
  let node = Rep { ty = TVar tvar_id; rank = 0 } in
  set_union_find_node ~cx tvar_id node;
  node

let lookup_union_find_node ~cx tvar_id =
  match IMap.find_opt tvar_id cx.union_forest_nodes with
  | Some node -> node
  | None -> add_tvar ~cx tvar_id

let rec find_union_rep_node ~cx tvar_id =
  match lookup_union_find_node ~cx tvar_id with
  | Rep { ty; rank } -> (tvar_id, ty, rank)
  | Link next_tvar_id ->
    let ((rep_id, _, _) as result) = find_union_rep_node ~cx next_tvar_id in
    set_union_find_node ~cx tvar_id (Link rep_id);
    result

let find_rep_tvar_id ~cx tvar_id =
  let (rep_tvar_id, _, _) = find_union_rep_node ~cx tvar_id in
  rep_tvar_id

let rec find_rep_type ~cx ty =
  match ty with
  | Unit
  | Bool
  | Int
  | String ->
    ty
  | Function { params; return } ->
    let params' = id_map_list (find_rep_type ~cx) params in
    let return' = find_rep_type ~cx return in
    if params == params' && return == return' then
      ty
    else
      Function { params = params'; return = return' }
  | TVar tvar_id ->
    let (_, rep_ty, _) = find_union_rep_node ~cx tvar_id in
    (match rep_ty with
    | TVar rep_tvar_id ->
      if tvar_id = rep_tvar_id then
        ty
      else
        rep_ty
    | _ -> find_rep_type ~cx rep_ty)

let union_tvars ~cx ty1 ty2 =
  match (ty1, ty2) with
  | (TVar tvar1, TVar tvar2) when tvar1 = tvar2 -> true
  | (TVar tvar1, TVar tvar2) ->
    let (rep_id1, _, rank1) = find_union_rep_node ~cx tvar1 in
    let (rep_id2, _, rank2) = find_union_rep_node ~cx tvar2 in
    let (child_id, rep_id) =
      if rank1 < rank2 then
        (rep_id1, rep_id2)
      else (
        if rank1 = rank2 then set_union_find_node ~cx rep_id1 (Rep { ty = ty1; rank = rank1 + 1 });
        (rep_id2, rep_id1)
      )
    in
    set_union_find_node ~cx child_id (Link rep_id);
    true
  | (TVar tvar, ty)
  | (ty, TVar tvar) ->
    let (rep_id, _, rank) = find_union_rep_node ~cx tvar in
    let rep_ty = find_rep_type ~cx ty in
    if Types.tvar_occurs_in rep_id rep_ty then
      false
    else (
      set_union_find_node ~cx tvar (Rep { ty = rep_ty; rank = rank + 1 });
      true
    )
  | _ -> failwith "At least one argument to union must be a tvar"

let find_union_rep_type ~cx ty =
  match ty with
  | TVar tvar_id ->
    let (_, rep_ty, _) = find_union_rep_node ~cx tvar_id in
    rep_ty
  | _ -> ty

let rec unify ~cx ty1 ty2 =
  let rep_ty1 = find_union_rep_type ~cx ty1 in
  let rep_ty2 = find_union_rep_type ~cx ty2 in
  match (rep_ty1, rep_ty2) with
  | (TVar _, _)
  | (_, TVar _) ->
    union_tvars ~cx rep_ty1 rep_ty2
  | (Unit, Unit)
  | (Bool, Bool)
  | (Int, Int)
  | (String, String) ->
    true
  | ( Function { params = params1; return = return1 },
      Function { params = params2; return = return2 } ) ->
    List.length params1 = List.length params2
    && List.combine params1 params2 |> List.for_all (fun (ty1, ty2) -> unify ~cx ty1 ty2)
    && unify ~cx return1 return2
  | _ -> false
