open Basic_collections
open Bindings
open Immutable_utils
open Types

type t = {
  mutable bindings: Bindings.t;
  mutable errors: (Loc.t * Analyze_error.t) list;
  mutable loc_to_tvar: TVar.t LocMap.t;
  mutable union_forest_nodes: union_forest_node IMap.t;
  (* Map of return node locs to the return type for that function *)
  mutable return_types: Type.t LocMap.t;
  (* Set of all int literal locs that have not been resolved *)
  mutable unresolved_int_literals: LocSet.t;
}

and union_forest_node =
  | Rep of {
      ty: Type.t;
      rank: int;
    }
  | Link of TVar.t

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

let get_errors ~cx = cx.errors

let set_errors ~cx errors = cx.errors <- errors

let add_return_type ~cx loc return_type =
  cx.return_types <- LocMap.add loc return_type cx.return_types

let get_return_types ~cx = cx.return_types

let get_unresolved_int_literals ~cx = cx.unresolved_int_literals

let get_value_binding ~cx use_loc = get_value_binding cx.bindings use_loc

let get_type_binding ~cx use_loc = get_type_binding cx.bindings use_loc

let get_type_binding_from_decl ~cx use_loc = get_type_binding_from_decl cx.bindings use_loc

let get_tvar_from_loc ~cx loc = LocMap.find loc cx.loc_to_tvar

let get_tvar_from_loc_opt ~cx loc = LocMap.find_opt loc cx.loc_to_tvar

let set_tvar_for_loc ~cx tvar loc = cx.loc_to_tvar <- LocMap.add loc tvar cx.loc_to_tvar

let set_union_find_node ~cx tvar_id node =
  cx.union_forest_nodes <- IMap.add tvar_id node cx.union_forest_nodes

let add_tvar ~cx tvar_id =
  let node = Rep { ty = TVar tvar_id; rank = 0 } in
  set_union_find_node ~cx tvar_id node;
  node

let mk_tvar_id ~cx ~loc =
  let tvar_id = TVar.mk () in
  ignore (add_tvar ~cx tvar_id);
  set_tvar_for_loc ~cx tvar_id loc;
  tvar_id

(* Find representative type for a given type following rep chains within type instead of
   union forest. At the moment this only resolves int literal types to their representative type. *)
let rec find_non_union_rep_type ty =
  match ty with
  | Type.IntLiteral ({ resolved = Some ty; _ } as lit_ty) ->
    let ty = find_non_union_rep_type ty in
    lit_ty.resolved <- Some ty;
    ty
  | _ -> ty

(* Combine two int literal types ty1 and ty2, which makes ty2 the representative of ty1 and moves
   all referenced int literals from ty1 to ty2. *)
let union_int_literals (ty1 : IntLiteral.t) (ty2 : IntLiteral.t) ty2_full =
  (* Already unioned if same type variables *)
  if ty1 != ty2 then (
    ty2.values <- ty1.values @ ty2.values;
    ty1.values <- [];
    ty1.resolved <- Some ty2_full
  )

(* Resolve an int literal with an integer type, which will set the representative type for the
   int literal type and error for each referenced int literal that is out of range. *)
let resolve_int_literal ~cx (lit_ty : IntLiteral.t) (ty : Type.t) =
  List.iter
    (fun (loc, value_opt) ->
      let is_out_of_range =
        match value_opt with
        | None -> true
        | Some value ->
          (match ty with
          | Byte -> Integers.is_out_of_signed_byte_range value
          | Int -> Integers.is_out_of_signed_int_range value
          | Long -> false
          | _ -> failwith "Int literal must be resolved to an int")
      in
      if is_out_of_range then add_error ~cx loc (Analyze_error.IntLiteralOutOfRange ty);
      cx.unresolved_int_literals <- LocSet.remove loc cx.unresolved_int_literals)
    lit_ty.values;
  lit_ty.resolved <- Some ty

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

let find_union_rep_type ~cx (ty : Type.t) =
  match ty with
  | TVar tvar_id ->
    let (_, rep_ty, _) = find_union_rep_node ~cx tvar_id in
    rep_ty
  | _ -> find_non_union_rep_type ty

let rec find_rep_type ~cx (ty : Type.t) =
  match ty with
  | Any
  | Unit
  | Bool
  | Byte
  | Int
  | Long
  | IntLiteral { resolved = None; _ }
  | TypeParam _ ->
    ty
  | IntLiteral { resolved = Some ty; _ } -> find_non_union_rep_type ty
  | Array element ->
    let element' = find_rep_type ~cx element in
    if element == element' then
      ty
    else
      Array element'
  | Tuple elements ->
    let elements' = id_map_list (find_rep_type ~cx) elements in
    if elements == elements' then
      ty
    else
      Tuple elements'
  | Function { type_args; params; return } ->
    let params' = id_map_list (find_rep_type ~cx) params in
    let return' = find_rep_type ~cx return in
    if params == params' && return == return' then
      ty
    else
      Function { type_args; params = params'; return = return' }
  | ADT { adt_sig; type_args } ->
    let type_args' = id_map_list (find_rep_type ~cx) type_args in
    if type_args == type_args' then
      ty
    else
      ADT { adt_sig; type_args = type_args' }
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
  | Byte
  | Int
  | Long
  | IntLiteral _
  | TypeParam _ ->
    false
  | Array element -> tvar_occurs_in ~cx tvar element
  | Tuple elements -> List.exists (tvar_occurs_in ~cx tvar) elements
  | Function { type_args = _; params; return } ->
    List.exists (tvar_occurs_in ~cx tvar) params || tvar_occurs_in ~cx tvar return
  | ADT { adt_sig = _; type_args } -> List.exists (tvar_occurs_in ~cx tvar) type_args
  | TVar rep_tvar -> tvar = rep_tvar

let union_tvars ~cx (ty1 : Type.t) (ty2 : Type.t) =
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
  | (Byte, Byte)
  | (Int, Int)
  | (Long, Long) ->
    true
  (* Type parameters check that they are identical *)
  | (TypeParam { id = id1; name = _ }, TypeParam { id = id2; name = _ }) -> id1 = id2
  (* Arrays unify their element types *)
  | (Array element1, Array element2) -> unify ~cx element1 element2
  (* Tuples unify all their elements if they have the same arity *)
  | (Tuple elements1, Tuple elements2) ->
    List.length elements1 = List.length elements2
    && List.for_all2 (fun ty1 ty2 -> unify ~cx ty1 ty2) elements1 elements2
  (* Functions unify all their parameter and return types if they have the same arity and TypeParams *)
  | ( Function { type_args = _; params = params1; return = return1 },
      Function { type_args = _; params = params2; return = return2 } ) ->
    List.length params1 = List.length params2
    && List.for_all2 (fun ty1 ty2 -> unify ~cx ty1 ty2) params1 params2
    && unify ~cx return1 return2
  (* Algebraic data types must have same signature and type params *)
  | (ADT { adt_sig = adt_sig1; type_args = args1 }, ADT { adt_sig = adt_sig2; type_args = args2 })
    ->
    adt_sig1 == adt_sig2
    && List.length args1 = List.length args2
    && List.for_all2 (fun ty1 ty2 -> unify ~cx ty1 ty2) args1 args2
  (* Unresolved int literals can be unified *)
  | (IntLiteral lit_ty1, (IntLiteral lit_ty2 as ty2)) ->
    union_int_literals lit_ty1 lit_ty2 ty2;
    true
  (* An unresolved int literal can be unified with any integer type *)
  | (((Byte | Int | Long) as ty), IntLiteral lit_ty)
  | (IntLiteral lit_ty, ((Byte | Int | Long) as ty)) ->
    resolve_int_literal ~cx lit_ty ty;
    true
  (* All other combinations of types cannot be unified *)
  | _ -> false

let rec is_subtype ~cx sub sup =
  let rep_sub = find_union_rep_type ~cx sub in
  let rep_sup = find_union_rep_type ~cx sup in
  match (rep_sub, rep_sup) with
  (* Type variables greedibly unify with each other when a subtype relation is applied *)
  | (TVar _, _)
  | (_, TVar _) ->
    unify ~cx rep_sub rep_sup
  (* The Any type allows all subtype relations *)
  | (Any, _)
  | (_, Any)
  | (Unit, Unit)
  | (Bool, Bool)
  | (Byte, Byte)
  | (Int, Int)
  | (Long, Long) ->
    true
  (* Type parameters are invariant, so check that they are identical *)
  | (TypeParam { id = id1; name = _ }, TypeParam { id = id2; name = _ }) -> id1 = id2
  (* Array type parameter is invariant *)
  | (Array element1, Array element2) ->
    is_subtype ~cx element1 element2 && is_subtype ~cx element2 element1
  (* Tuple element types are covariant *)
  | (Tuple sub_elements, Tuple sup_elements) ->
    List.length sub_elements = List.length sup_elements
    && List.for_all2 (fun sub sup -> is_subtype ~cx sup sub) sub_elements sup_elements
  (* Function parameters are contravariant and return type is covariant. Type parameters are
     ignored when checking subtyping, as long as parameters and return types correctly subtype. *)
  | ( Function { type_args = _; params = sub_params; return = sub_return },
      Function { type_args = _; params = sup_params; return = sup_return } ) ->
    List.length sub_params = List.length sup_params
    && List.for_all2 (fun sub sup -> is_subtype ~cx sup sub) sub_params sup_params
    && is_subtype ~cx sub_return sup_return
  (* Algebraic type parameters are invariant *)
  | (ADT { adt_sig = adt_sig1; type_args = args1 }, ADT { adt_sig = adt_sig2; type_args = args2 })
    ->
    adt_sig1 == adt_sig2
    && List.length args1 = List.length args2
    && List.for_all2 (fun ty1 ty2 -> is_subtype ~cx ty1 ty2 && is_subtype ~cx ty2 ty1) args1 args2
  (* Int literals are not subtyped so they must be unified *)
  | (IntLiteral lit_ty1, (IntLiteral lit_ty2 as ty2)) ->
    union_int_literals lit_ty1 lit_ty2 ty2;
    true
  (* Integer types are not subtyped so unify types *)
  | (((Byte | Int | Long) as ty), IntLiteral lit_ty)
  | (IntLiteral lit_ty, ((Byte | Int | Long) as ty)) ->
    resolve_int_literal ~cx lit_ty ty;
    true
  | _ -> false

let add_incompatible_types_error ~cx loc ty1 ty2 =
  add_error ~cx loc (IncompatibleTypes (find_rep_type ~cx ty1, [find_rep_type ~cx ty2]))

let assert_unify ~cx loc expected actual =
  if not (unify ~cx expected actual) then add_incompatible_types_error ~cx loc actual expected

let assert_is_subtype ~cx loc sub sup =
  if not (is_subtype ~cx sub sup) then add_incompatible_types_error ~cx loc sub sup

let mk_int_literal_ty ~cx loc raw base =
  cx.unresolved_int_literals <- LocSet.add loc cx.unresolved_int_literals;
  let value = Integers.int64_of_string_opt raw base in
  Type.IntLiteral { values = [(loc, value)]; resolved = None }
