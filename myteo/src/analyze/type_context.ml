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
  (* Set of all int literal locs that have not been resolved *)
  mutable unresolved_int_literals: LocSet.t;
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
    unresolved_int_literals = LocSet.empty;
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

let mk_tvar_id ~cx ~loc =
  let tvar_id = Types.mk_tvar_id () in
  ignore (add_tvar ~cx tvar_id);
  set_tvar_for_loc ~cx tvar_id loc;
  tvar_id

(* Find representative type for a given type following rep chains within type instead of
   union forest. At the moment this only resolves int literal types to their representative type. *)
let rec find_non_union_rep_type ty =
  match ty with
  | IntLiteral ({ resolved = Some ty; _ } as lit_ty) ->
    let ty = find_non_union_rep_type ty in
    lit_ty.resolved <- Some ty;
    ty
  | _ -> ty

(* Combine two int literal types ty1 and ty2, which makes ty2 the representative of ty1 and moves
   all referenced int literals from ty1 to ty2. *)
let union_int_literals (ty1 : Types.int_literal) (ty2 : Types.int_literal) ty2_full =
  (* Already unioned if same type variables *)
  if ty1 != ty2 then (
    ty2.values <- ty1.values @ ty2.values;
    ty1.values <- [];
    ty1.resolved <- Some ty2_full
  )

(* Resolve an int literal with an integer type, which will set the reperesentative type for the
   int literal type and error for each referenced int literal that is out of range. *)
let resolve_int_literal ~cx lit_ty =
  List.iter
    (fun (loc, value_opt) ->
      let is_out_of_range =
        match value_opt with
        | None -> true
        | Some value ->
          Int64.compare (Int64.of_int32 Int32.min_int) value = 1
          || Int64.compare (Int64.of_int32 Int32.max_int) value = -1
      in
      if is_out_of_range then add_error ~cx loc (Analyze_error.IntLiteralOutOfRange Int);
      cx.unresolved_int_literals <- LocSet.remove loc cx.unresolved_int_literals)
    lit_ty.values;
  lit_ty.resolved <- Some Int

let lookup_union_find_node ~cx tvar_id =
  match IMap.find_opt tvar_id cx.union_forest_nodes with
  | Some node -> node
  | None -> add_tvar ~cx tvar_id

let find_union_rep_node ~cx tvar_id =
  let rec helper ~cx tvar_id =
    match lookup_union_find_node ~cx tvar_id with
    | Rep { ty; rank } -> (tvar_id, ty, rank)
    | Link next_tvar_id ->
      let ((rep_id, _, _) as result) = helper ~cx next_tvar_id in
      set_union_find_node ~cx tvar_id (Link rep_id);
      result
  in
  let (rep_id, ty, rank) = helper ~cx tvar_id in
  (rep_id, find_non_union_rep_type ty, rank)

let find_union_rep_type ~cx ty =
  match ty with
  | TVar tvar_id ->
    let (_, rep_ty, _) = find_union_rep_node ~cx tvar_id in
    rep_ty
  | _ -> find_non_union_rep_type ty

let find_rep_tvar_id ~cx tvar_id =
  let (rep_tvar_id, _, _) = find_union_rep_node ~cx tvar_id in
  rep_tvar_id

let rec find_rep_type ~cx ty =
  match ty with
  | Any
  | Unit
  | Bool
  | Int
  | IntLiteral { resolved = None; _ }
  | String ->
    ty
  | IntLiteral { resolved = Some ty; _ } -> find_non_union_rep_type ty
  | Tuple elements ->
    let elements' = id_map_list (find_rep_type ~cx) elements in
    if elements == elements' then
      ty
    else
      Tuple elements'
  | Function { params; return } ->
    let params' = id_map_list (find_rep_type ~cx) params in
    let return' = find_rep_type ~cx return in
    if params == params' && return == return' then
      ty
    else
      Function { params = params'; return = return' }
  | ADT { adt_sig; tparams } ->
    let tparams' = id_map_list (find_rep_type ~cx) tparams in
    if tparams == tparams' then
      ty
    else
      ADT { adt_sig; tparams = tparams' }
  | TVar tvar_id ->
    let (_, rep_ty, _) = find_union_rep_node ~cx tvar_id in
    (match rep_ty with
    | TVar rep_tvar_id ->
      if tvar_id = rep_tvar_id then
        ty
      else
        rep_ty
    | _ -> find_rep_type ~cx rep_ty)

let rec tvar_occurs_in ~cx tvar ty =
  match find_union_rep_type ~cx ty with
  | Any
  | Unit
  | Bool
  | Int
  | IntLiteral _
  | String ->
    false
  | Tuple elements -> List.exists (tvar_occurs_in ~cx tvar) elements
  | Function { params; return } ->
    List.exists (tvar_occurs_in ~cx tvar) params || tvar_occurs_in ~cx tvar return
  | ADT { adt_sig = _; tparams } -> List.exists (tvar_occurs_in ~cx tvar) tparams
  | TVar rep_tvar -> tvar = rep_tvar

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
    let (rep_tvar, _, rank) = find_union_rep_node ~cx tvar in
    if tvar_occurs_in ~cx rep_tvar ty then
      false
    else
      let rep_ty = find_rep_type ~cx ty in
      set_union_find_node ~cx tvar (Rep { ty = rep_ty; rank = rank + 1 });
      true
  | _ -> failwith "At least one argument to union must be a tvar"

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
  (* Tuples unify all their elements if they have the same arity *)
  | (Tuple elements1, Tuple elements2) ->
    List.length elements1 = List.length elements2
    && List.combine elements1 elements2 |> List.for_all (fun (ty1, ty2) -> unify ~cx ty1 ty2)
  (* Functions unify all their parameter and return types if they have the same arity *)
  | ( Function { params = params1; return = return1 },
      Function { params = params2; return = return2 } ) ->
    List.length params1 = List.length params2
    && List.combine params1 params2 |> List.for_all (fun (ty1, ty2) -> unify ~cx ty1 ty2)
    && unify ~cx return1 return2
  (* Algebraic data types must have same signature and type params *)
  | (ADT { adt_sig = adt_sig1; tparams = tparams1 }, ADT { adt_sig = adt_sig2; tparams = tparams2 })
    ->
    adt_sig1 == adt_sig2
    && List.combine tparams1 tparams2 |> List.for_all (fun (ty1, ty2) -> unify ~cx ty1 ty2)
  (* Unresolved int literals can be unified *)
  | (IntLiteral lit_ty1, (IntLiteral lit_ty2 as ty2)) ->
    union_int_literals lit_ty1 lit_ty2 ty2;
    true
  (* An unresolved int literal can be unified with any integer type *)
  | (Int, IntLiteral lit_ty)
  | (IntLiteral lit_ty, Int) ->
    resolve_int_literal ~cx lit_ty;
    true
  (* All other combinations of types cannot be unified *)
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
  (* Tuple element types are covariant *)
  | (Tuple sub_elements, Tuple sup_elements) ->
    List.length sub_elements = List.length sup_elements
    && List.combine sub_elements sup_elements
       |> List.for_all (fun (sub, sup) -> is_subtype ~cx sup sub)
  (* Function parameters are contravariant and return type is covariant *)
  | ( Function { params = sub_params; return = sub_return },
      Function { params = sup_params; return = sup_return } ) ->
    List.length sub_params = List.length sup_params
    && List.combine sub_params sup_params |> List.for_all (fun (sub, sup) -> is_subtype ~cx sup sub)
    && is_subtype ~cx sub_return sup_return
  (* Algebraic type parameters are invariant *)
  | (ADT { adt_sig = adt_sig1; tparams = tparams1 }, ADT { adt_sig = adt_sig2; tparams = tparams2 })
    ->
    adt_sig1 == adt_sig2
    && List.combine tparams1 tparams2
       |> List.for_all (fun (ty1, ty2) -> is_subtype ~cx ty1 ty2 && is_subtype ~cx ty2 ty1)
  (* Int literals are not subtyped so they must be unified *)
  | (IntLiteral lit_ty1, (IntLiteral lit_ty2 as ty2)) ->
    union_int_literals lit_ty1 lit_ty2 ty2;
    true
  (* Integer types are not subtyped so unify types *)
  | (Int, IntLiteral lit_ty)
  | (IntLiteral lit_ty, Int) ->
    resolve_int_literal ~cx lit_ty;
    true
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

let mk_int_literal_ty ~cx loc raw =
  cx.unresolved_int_literals <- LocSet.add loc cx.unresolved_int_literals;
  IntLiteral { values = [(loc, Int64.of_string_opt raw)]; resolved = None }
