open Basic_collections
open Bindings
open Immutable_utils
open Types

type t = {
  bindings: Bindings.t;
  mutable errors: (Loc.t * Analyze_error.t) list;
  mutable loc_to_tvar: Types.tvar_id LocMap.t;
  mutable union_forest_nodes: union_forest_node IMap.t;
  (* Map of return node locs to the return type for that function *)
  mutable return_types: Types.t LocMap.t;
}

and union_forest_node =
  | Rep of {
      ty: Types.t;
      rank: int;
    }
  | Link of Types.tvar_id

let mk ~bindings =
  {
    bindings;
    errors = [];
    loc_to_tvar = LocMap.empty;
    union_forest_nodes = IMap.empty;
    return_types = LocMap.empty;
  }

let add_error ~cx loc error = cx.errors <- (loc, error) :: cx.errors

let get_source_value_binding ~cx use_loc = get_source_value_binding cx.bindings use_loc

let get_source_type_binding ~cx use_loc = get_source_type_binding cx.bindings use_loc

let get_tvar_id_from_value_decl ~cx decl_loc = get_tvar_id_from_value_decl cx.bindings decl_loc

let get_tvar_id_from_type_decl ~cx decl_loc = get_tvar_id_from_type_decl cx.bindings decl_loc

let get_tvar_id_from_value_use ~cx use_loc = get_tvar_id_from_value_use cx.bindings use_loc

let get_tvar_id_from_type_use ~cx use_loc = get_tvar_id_from_type_use cx.bindings use_loc

let get_tvar_from_loc ~cx loc = LocMap.find loc cx.loc_to_tvar

let set_tvar_for_loc ~cx tvar loc = cx.loc_to_tvar <- LocMap.add loc tvar cx.loc_to_tvar

let set_union_find_node ~cx tvar_id node =
  cx.union_forest_nodes <- IMap.add tvar_id node cx.union_forest_nodes

let add_tvar ~cx tvar_id =
  let node = Rep { ty = TVar tvar_id; rank = 0 } in
  set_union_find_node ~cx tvar_id node;
  node

let mk_tvar_id ?loc ~cx =
  let tvar_id = Types.mk_tvar_id () in
  ignore (add_tvar ~cx tvar_id);
  begin
    match loc with
    | None -> ()
    | Some loc -> set_tvar_for_loc ~cx tvar_id loc
  end;
  tvar_id

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
  | Any
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
  | (Any, _)
  | (_, Any)
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

let rec is_subtype ~cx sub sup =
  let rep_sub = find_union_rep_type ~cx sub in
  let rep_sup = find_union_rep_type ~cx sup in
  match (rep_sub, rep_sup) with
  | (TVar _, _)
  | (_, TVar _) ->
    (* TODO: Error on unresolved tvars. Should check for nested unresolved tvars to
             see if type is fully resolved at start of is_subtype. Error if not, pointing
             to type and displaying as much of type as could be resolved in error, saying to provide
             annotation for unannotated parts. If entire tvar cannot be resolved and there is
             no useful structure to show, just say to add annotations)*)
    failwith "Unimplemented"
  | (Any, _)
  | (_, Any)
  | (Unit, Unit)
  | (Bool, Bool)
  | (Int, Int)
  | (String, String) ->
    true
  | ( Function { params = sub_params; return = sub_return },
      Function { params = sup_params; return = sup_return } ) ->
    List.length sub_params = List.length sup_params
    && List.combine sub_params sup_params |> List.for_all (fun (sub, sup) -> is_subtype ~cx sup sub)
    && is_subtype ~cx sub_return sup_return
  | _ -> false

let assert_unify ~cx loc expected actual =
  if not (unify ~cx expected actual) then
    add_error ~cx loc (IncompatibleTypes (find_rep_type ~cx actual, [find_rep_type ~cx expected]))

let assert_is_subtype ~cx loc sub sup =
  if not (is_subtype ~cx sub sup) then
    add_error
      ~cx
      loc
      (Analyze_error.IncompatibleTypes (find_rep_type ~cx sub, [find_rep_type ~cx sup]))
