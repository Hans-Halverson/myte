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
  (* Set of all method uses locs *)
  mutable method_uses: LocSet.t;
  mutable main_loc: Loc.t;
  (* Order of (non-type) trait declarations in current compilation unit *)
  mutable ordered_traits: Ast.TraitDeclaration.t list;
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
    method_uses = LocSet.empty;
    main_loc = Loc.none;
    ordered_traits = [];
  }

let add_error ~cx loc error = cx.errors <- (loc, error) :: cx.errors

let get_errors ~cx = cx.errors

let set_errors ~cx errors = cx.errors <- errors

let add_return_type ~cx loc return_type =
  cx.return_types <- LocMap.add loc return_type cx.return_types

let get_return_types ~cx = cx.return_types

let get_unresolved_int_literals ~cx = cx.unresolved_int_literals

let add_method_use ~cx use_loc = cx.method_uses <- LocSet.add use_loc cx.method_uses

let is_method_use ~cx use_loc = LocSet.mem use_loc cx.method_uses

let set_main_loc ~cx main_loc = cx.main_loc <- main_loc

let is_main_loc ~cx loc = cx.main_loc == loc

let get_ordered_traits ~cx = cx.ordered_traits

let set_ordered_traits ~cx ordered_traits = cx.ordered_traits <- ordered_traits

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

(* Combine two int literal types ty1 and ty2, which makes ty2 the representative of ty1 and moves
   all referenced int literals from ty1 to ty2. *)
let union_int_literals (ty1 : IntLiteral.t) (ty2 : IntLiteral.t) ty2_full =
  (* Already unioned if same type variables *)
  if ty1 != ty2 then (
    ty2.values <- ty1.values @ ty2.values;
    ty1.values <- [];
    ty1.resolved <- Some ty2_full
  )

(* Combine two int literal types ty1 and ty2, which makes ty2 the representative of ty1 and moves
   all referenced int literals from ty1 to ty2. *)
let union_trait_bounds (ty1 : TraitBound.t) (ty2 : TraitBound.t) ty2_full =
  (* Already unioned if same type variables *)
  if ty1 != ty2 then (
    ty2.bounds <- ty1.bounds @ ty2.bounds;
    ty1.bounds <- [];
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

(* Resolve an int literal type to an integer type. Default to Int if all literal values are within
   the Int range, otherwise use Long. *)
let resolve_int_literal_from_values ~cx (lit_ty : IntLiteral.t) =
  let resolved_ty =
    List.fold_left
      (fun resolved_ty (_, value) ->
        match value with
        | Some value when Integers.is_out_of_signed_int_range value -> Type.Long
        | Some _
        | None ->
          resolved_ty)
      Type.Int
      lit_ty.values
  in
  resolve_int_literal ~cx lit_ty resolved_ty;
  resolved_ty

let lookup_union_find_node ~cx tvar_id =
  match IMap.find_opt tvar_id cx.union_forest_nodes with
  | Some node -> node
  | None -> add_tvar ~cx tvar_id

let rec find_union_rep_type ~cx (ty : Type.t) =
  match ty with
  | TVar tvar_id ->
    let (_, rep_ty, _) = find_union_rep_node ~cx tvar_id in
    rep_ty
  | _ -> find_non_union_rep_type ~cx ty

and find_union_rep_node ~cx tvar_id =
  let rec helper ~cx tvar_id =
    match lookup_union_find_node ~cx tvar_id with
    | Rep { ty; rank } -> (tvar_id, ty, rank)
    | Link next_tvar_id ->
      let ((rep_id, _, _) as result) = helper ~cx next_tvar_id in
      set_union_find_node ~cx tvar_id (Link rep_id);
      result
  in
  let (rep_id, ty, rank) = helper ~cx tvar_id in
  (rep_id, find_non_union_rep_type ~cx ty, rank)

(* Find representative type for a given type following rep chains within type instead of
   union forest. This resolves int literal and trait bound types to their representative type. *)
and find_non_union_rep_type ~cx ty =
  match ty with
  | Type.IntLiteral ({ resolved = Some ty; _ } as lit_ty) ->
    let ty = find_union_rep_type ~cx ty in
    lit_ty.resolved <- Some ty;
    ty
  | TraitBound ({ resolved = Some ty; _ } as trait_bound_ty) ->
    let ty = find_union_rep_type ~cx ty in
    trait_bound_ty.resolved <- Some ty;
    ty
  | _ -> ty

let rec find_rep_type ~cx (ty : Type.t) =
  let find_trait_instance_rep_type ~cx ({ TraitSig.trait_sig; type_args } as instance) =
    let type_args' = id_map_list (find_rep_type ~cx) type_args in
    if type_args == type_args' then
      instance
    else
      { TraitSig.trait_sig; type_args = type_args' }
  in
  match ty with
  | Any
  | Unit
  | Bool
  | Byte
  | Int
  | Long
  | IntLiteral { resolved = None; _ } ->
    ty
  | IntLiteral { resolved = Some ty; _ }
  | TraitBound { resolved = Some ty; _ } ->
    find_non_union_rep_type ~cx ty
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
  | TraitBound ({ resolved = None; bounds } as trait_bound) ->
    (* Unresolved trait bound types must be preserved, so keep type and update bounds with rep types *)
    let bounds' = id_map_list (find_trait_instance_rep_type ~cx) bounds in
    trait_bound.bounds <- bounds';
    ty
  | TypeParam { id; name; bounds } ->
    let bounds' = id_map_list (find_trait_instance_rep_type ~cx) bounds in
    if bounds == bounds' then
      ty
    else
      TypeParam { id; name; bounds = bounds' }
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
  | TraitBound { bounds; resolved = _ } ->
    List.exists
      (fun { TraitSig.type_args; _ } -> List.exists (tvar_occurs_in ~cx tvar) type_args)
      bounds

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

let rec type_satisfies_trait_bounds ~cx ty trait_bounds =
  let open TraitSig in
  let trait_satisfies_bound trait bound type_param_bindings =
    let trait_match = bound.trait_sig.id = trait.trait_sig.id in
    if trait_match then
      List.for_all2
        (fun implemented_arg bound_arg ->
          let implemented_arg = Types.substitute_type_params type_param_bindings implemented_arg in
          unify ~cx implemented_arg bound_arg)
        trait.type_args
        bound.type_args
    else
      false
  in

  (* Traits can implement bounds directly if the trait matches the bound, otherwise check the
     implemented traits for each trait against the bound. *)
  let traits_satisfy_bounds traits bounds =
    List.for_all
      (fun bound ->
        List.exists
          (fun trait ->
            let is_satisfied = trait_satisfies_bound trait bound IMap.empty in
            if is_satisfied then
              true
            else
              (* Must substitute type args of trait within implemented traits *)
              let type_param_bindings =
                Types.bind_type_params_to_args trait.trait_sig.type_params trait.type_args
              in
              List.exists
                (fun (_, implemented_trait) ->
                  trait_satisfies_bound implemented_trait bound type_param_bindings)
                trait.trait_sig.implemented)
          traits)
      bounds
  in

  (* Check all implemented traits for ADT (across all type trait blocks) *)
  let adt_satisfies_bounds adt_sig type_args bounds =
    List.for_all
      (fun bound ->
        List.exists
          (fun type_trait ->
            (* Must substitute type args of ADT within type traits *)
            let type_param_bindings =
              Types.bind_type_params_to_args type_trait.type_params type_args
            in
            List.exists
              (fun (_, implemented_trait) ->
                trait_satisfies_bound implemented_trait bound type_param_bindings)
              type_trait.implemented)
          adt_sig.AdtSig.traits)
      bounds
  in

  match ty with
  (* The any type implements all traits *)
  | Type.Any -> true
  (* Look up known ADT sig in stdlib for each primitive type *)
  | Unit
  | Bool
  | Byte
  | Int
  | Long ->
    adt_satisfies_bounds (Std_lib.get_primitive_adt_sig ty) [] trait_bounds
  | TypeParam { bounds; _ } -> traits_satisfy_bounds bounds trait_bounds
  | ADT { adt_sig; type_args } -> adt_satisfies_bounds adt_sig type_args trait_bounds
  (* Unresolved int literal types are first resolved to the best choice based on their values *)
  | IntLiteral lit_ty ->
    let resolved_ty = resolve_int_literal_from_values ~cx lit_ty in
    type_satisfies_trait_bounds ~cx resolved_ty trait_bounds
  (* Types that do not implement any traits themselves *)
  | Array _
  | Tuple _
  | Function _ ->
    false
  | TVar _
  | TraitBound _ ->
    failwith "Already handled by unify or is_subtype"

and unify ~cx ty1 ty2 =
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
  | (TypeParam { id = id1; name = _; bounds = _ }, TypeParam { id = id2; name = _; bounds = _ }) ->
    id1 = id2
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
  (* Unresolved trait bounds can be unified *)
  | (TraitBound bound_ty1, (TraitBound bound_ty2 as ty2)) ->
    union_trait_bounds bound_ty1 bound_ty2 ty2;
    true
  (* Unresolved trait bounds can be unified with a concrete type if the type satisfies the bound *)
  | (ty, TraitBound trait_bound)
  | (TraitBound trait_bound, ty) ->
    let rep_ty = find_rep_type ~cx ty in
    let rep_trait_bound =
      match find_rep_type ~cx (TraitBound trait_bound) with
      | TraitBound trait_bound -> trait_bound
      | _ -> failwith "Expected trait_bound"
    in
    let satisfies_trait_bound = type_satisfies_trait_bounds ~cx rep_ty rep_trait_bound.bounds in
    if satisfies_trait_bound then rep_trait_bound.resolved <- Some rep_ty;
    satisfies_trait_bound
  (* All other combinations of types cannot be unified *)
  | _ -> false

and is_subtype ~cx sub sup =
  let rep_sub = find_union_rep_type ~cx sub in
  let rep_sup = find_union_rep_type ~cx sup in
  match (rep_sub, rep_sup) with
  (* Type variables greedily unify with each other when a subtype relation is applied *)
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
  | (TypeParam { id = id1; name = _; bounds = _ }, TypeParam { id = id2; name = _; bounds = _ }) ->
    id1 = id2
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
  (* Unresolved trait bounds are not subtyped so they must be unified *)
  | (TraitBound bound_ty1, (TraitBound bound_ty2 as ty2)) ->
    union_trait_bounds bound_ty1 bound_ty2 ty2;
    true
  (* Unresolved trait bounds can be unified with a concrete type if the type satisfies the bound *)
  | (ty, TraitBound trait_bound) ->
    let rep_ty = find_rep_type ~cx ty in
    let rep_trait_bound =
      match find_rep_type ~cx (TraitBound trait_bound) with
      | TraitBound trait_bound -> trait_bound
      | _ -> failwith "Expected trait_bound"
    in
    let satisfies_trait_bound = type_satisfies_trait_bounds ~cx rep_ty rep_trait_bound.bounds in
    if satisfies_trait_bound then rep_trait_bound.resolved <- Some rep_ty;
    satisfies_trait_bound
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

let implements_trait ty trait_sig =
  let open TraitSig in
  let adt_sig_implements_trait adt_sig trait_sig =
    List.exists
      (fun { implemented; _ } ->
        List.exists (fun (_, implemented) -> implemented.trait_sig.id = trait_sig.id) implemented)
      adt_sig.AdtSig.traits
  in
  (* Bounds may implement the trait directly, or the trait may be in the bound's super traits *)
  let bounds_implements_trait bounds trait_sig =
    List.exists
      (fun { trait_sig = bound_trait_sig; _ } ->
        bound_trait_sig.id = trait_sig.id
        || List.exists
             (fun (_, implemented) -> implemented.trait_sig.id = trait_sig.id)
             bound_trait_sig.implemented)
      bounds
  in
  match ty with
  | Type.Any -> true
  | Unit
  | Bool
  | Byte
  | Int
  | Long ->
    let adt_sig = Std_lib.get_primitive_adt_sig ty in
    adt_sig_implements_trait adt_sig trait_sig
  | ADT { adt_sig; _ } -> adt_sig_implements_trait adt_sig trait_sig
  | TypeParam { bounds; _ } -> bounds_implements_trait bounds trait_sig
  | TraitBound { bounds; _ } -> bounds_implements_trait bounds trait_sig
  | _ -> false
