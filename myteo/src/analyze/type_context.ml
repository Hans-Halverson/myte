open Basic_collections
open Types

type t = { mutable union_forest_nodes: union_forest_node IMap.t }

and union_forest_node =
  | Rep of {
      ty: Types.t;
      rank: int;
    }
  | Link of Types.tvar_id

let lookup_union_find_node ~cx tvar_id = IMap.find tvar_id cx.union_forest_nodes

let set_union_find_node ~cx tvar_id node =
  cx.union_forest_nodes <- IMap.add tvar_id node cx.union_forest_nodes

let add_tvar ~cx tvar_id = set_union_find_node ~cx tvar_id (Rep { ty = TVar tvar_id; rank = 0 })

let rec find_union_rep_node ~cx tvar_id =
  match lookup_union_find_node ~cx tvar_id with
  | Rep { ty; rank } -> (tvar_id, ty, rank)
  | Link next_tvar_id ->
    let ((rep_id, _, _) as result) = find_union_rep_node ~cx next_tvar_id in
    set_union_find_node ~cx tvar_id (Link rep_id);
    result

let union_tvars ~cx tvar_id1 tvar_id2 =
  let (rep_id1, ty1, rank1) = find_union_rep_node ~cx tvar_id1 in
  let (rep_id2, ty2, rank2) = find_union_rep_node ~cx tvar_id2 in
  if rep_id1 != rep_id2 then (
    let (child_id, rep_id, new_rep_node) =
      match (ty1, ty2) with
      | (TVar _, TVar _) ->
        if rank1 < rank2 then
          (rep_id1, rep_id2, None)
        else
          ( rep_id2,
            rep_id1,
            if rank1 > rank2 then
              None
            else
              Some (rank1 + 1, ty1) )
      | (TVar _, _) ->
        ( rep_id1,
          rep_id2,
          if rank2 > rank1 then
            None
          else
            Some (rank1 + 1, ty2) )
      | (_, TVar _) ->
        ( rep_id2,
          rep_id1,
          if rank1 > rank2 then
            None
          else
            Some (rank2 + 1, ty1) )
      | _ -> failwith "At least one argument to union must be a tvar"
    in
    set_union_find_node ~cx child_id (Link rep_id);
    match new_rep_node with
    | None -> ()
    | Some (rank, ty) -> set_union_find_node ~cx rep_id (Rep { ty; rank })
  )

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
  | (TVar tvar_1, TVar tvar_2) ->
    if tvar_1 <> tvar_2 then union_tvars ~cx tvar_1 tvar_2;
    true
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
