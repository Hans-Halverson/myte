open Basic_collections

type tvar_id = int

type adt_sig_id = int

type t =
  | TVar of tvar_id
  | Any
  | Unit
  | Bool
  | Byte
  | Int
  | Long
  | IntLiteral of int_literal
  | String
  | Tuple of t list
  | Function of {
      params: t list;
      return: t;
    }
  | ADT of {
      adt_sig: adt_sig;
      tparams: t list;
    }

and int_literal = {
  (* Locations of int literals (and their tvars) along with the value of the int literal at
         that location (or None if it is out of range for 64 bit ints) *)
  mutable values: (Loc.t * Int64.t option) list;
  (* Integer type this int literal is resolved to, if it has been resolved *)
  mutable resolved: t option;
}

and adt_sig = {
  id: adt_sig_id;
  name: string;
  mutable tvar_sigs: tvar_id list;
  mutable variant_sigs: variant_sig SMap.t;
}

and variant_sig =
  | EnumVariantSig
  | TupleVariantSig of t list
  | RecordVariantSig of t SMap.t

let max_tvar_id = ref 0

let mk_tvar_id () =
  let tvar_id = !max_tvar_id in
  max_tvar_id := tvar_id + 1;
  tvar_id

let max_adt_sig_id = ref 0

let mk_adt_sig_id () =
  let adt_sig_id = !max_adt_sig_id in
  max_adt_sig_id := adt_sig_id + 1;
  adt_sig_id

let mk_tvar () = TVar (mk_tvar_id ())

let mk_adt_sig name = { id = mk_adt_sig_id (); name; tvar_sigs = []; variant_sigs = SMap.empty }

let get_all_tvars_with_duplicates ty =
  let rec inner acc ty =
    match ty with
    | TVar tvar_id -> tvar_id :: acc
    | Tuple elements -> List.fold_left inner acc elements
    | Function { params; return } ->
      let acc = List.fold_left inner acc params in
      inner acc return
    | ADT { tparams; _ } -> List.fold_left inner acc tparams
    | _ -> acc
  in
  inner [] ty |> List.rev

let get_all_tvars tys =
  let tvars_with_duplicates = List.map get_all_tvars_with_duplicates tys |> List.concat in
  let rec remove_duplicates seen rest =
    match rest with
    | [] -> []
    | hd :: tl ->
      if ISet.mem hd seen then
        remove_duplicates seen tl
      else
        hd :: remove_duplicates (ISet.add hd seen) tl
  in
  remove_duplicates ISet.empty tvars_with_duplicates

let concat_and_wrap (pre, post) elements = pre ^ String.concat ", " elements ^ post

let rec pp_with_names ~tvar_to_name ty =
  match ty with
  | Any -> "Any"
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Byte -> "Byte"
  | Int -> "Int"
  | Long -> "Long"
  | IntLiteral { resolved = None; _ } -> "<Integer>"
  | IntLiteral { resolved = Some resolved; _ } -> pp_with_names ~tvar_to_name resolved
  | String -> "String"
  | Tuple elements ->
    let element_names = List.map (pp_with_names ~tvar_to_name) elements in
    concat_and_wrap ("(", ")") element_names
  | Function { params; return } ->
    let pp_function_part ty =
      let pp_param = pp_with_names ~tvar_to_name ty in
      match ty with
      | Function _ -> "(" ^ pp_param ^ ")"
      | _ -> pp_param
    in
    let pp_params =
      match params with
      | [param] -> pp_function_part param
      | _ ->
        let pp_params = List.map (fun param -> pp_with_names ~tvar_to_name param) params in
        concat_and_wrap ("(", ")") pp_params
    in
    pp_params ^ " -> " ^ pp_function_part return
  | ADT { adt_sig = { name; _ }; tparams } ->
    if tparams = [] then
      name
    else
      let pp_tparams = List.map (pp_with_names ~tvar_to_name) tparams in
      concat_and_wrap (name ^ "<", ">") pp_tparams
  | TVar tvar_id ->
    let x = IMap.find tvar_id tvar_to_name in
    x

let name_id_to_string name_id =
  let quot = name_id / 26 in
  let rem = name_id mod 26 in
  let letter = Char.escaped (Char.chr (0x61 + rem)) in
  if quot = 0 then
    letter
  else
    letter ^ string_of_int (quot + 1)

let rec get_next_name next_name_id used_names =
  let name = name_id_to_string next_name_id in
  if SSet.mem name used_names then
    get_next_name (next_name_id + 1) used_names
  else
    (name, next_name_id + 1)

let get_adt_sig adt =
  match adt with
  | ADT { adt_sig; _ } -> adt_sig
  | _ -> failwith "Expected ADT"

let get_tuple_variant adt_sig name =
  match SMap.find name adt_sig.variant_sigs with
  | TupleVariantSig element_sigs -> element_sigs
  | _ -> failwith "Expected TupleVariantSig"

let get_record_variant adt_sig name =
  match SMap.find name adt_sig.variant_sigs with
  | RecordVariantSig field_sigs -> field_sigs
  | _ -> failwith "Expected RecordVariantSig"

let rec build_tvar_to_name ~use_tvar_ids (tvar_to_name_acc, names_acc) next_name_id tvar_ids =
  match tvar_ids with
  | [] -> (tvar_to_name_acc, names_acc)
  | hd :: tl ->
    if IMap.mem hd tvar_to_name_acc then
      build_tvar_to_name ~use_tvar_ids (tvar_to_name_acc, names_acc) next_name_id tl
    else
      let (next_name, next_name_id) =
        if use_tvar_ids then
          (string_of_int hd, next_name_id)
        else
          get_next_name next_name_id names_acc
      in
      build_tvar_to_name
        ~use_tvar_ids
        (IMap.add hd next_name tvar_to_name_acc, SSet.add next_name names_acc)
        next_name_id
        tl

let pps_with_tvar_map ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) tys =
  let all_tvars = get_all_tvars tys in
  let used_names = IMap.fold (fun _ name acc -> SSet.add name acc) tvar_to_name SSet.empty in
  let (tvar_to_name, _) = build_tvar_to_name ~use_tvar_ids (tvar_to_name, used_names) 0 all_tvars in
  (List.map (pp_with_names ~tvar_to_name) tys, tvar_to_name)

let pps ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) tys =
  fst (pps_with_tvar_map ~tvar_to_name ~use_tvar_ids tys)

let pp ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) ty =
  pps ~tvar_to_name ~use_tvar_ids [ty] |> List.hd

type type_hash_type_t = t

module TypeHash = struct
  type t = type_hash_type_t

  let rec equal (ty1 : t) (ty2 : t) =
    match (ty1, ty2) with
    | (TVar tvar1, TVar tvar2) -> tvar1 = tvar2
    | (Any, Any)
    | (Unit, Unit)
    | (Bool, Bool)
    | (Byte, Byte)
    | (Int, Int)
    | (Long, Long)
    | (String, String) ->
      true
    | (Tuple elements1, Tuple elements2) ->
      List.length elements1 = List.length elements2 && List.for_all2 equal elements1 elements2
    | ( Function { params = params1; return = return1 },
        Function { params = params2; return = return2 } ) ->
      List.length params1 = List.length params2
      && List.for_all2 equal params1 params2
      && equal return1 return2
    | ( ADT { adt_sig = adt_sig1; tparams = tparams1 },
        ADT { adt_sig = adt_sig2; tparams = tparams2 } ) ->
      adt_sig1 == adt_sig2 && List.for_all2 equal tparams1 tparams2
    | (IntLiteral { resolved = None; _ }, IntLiteral { resolved = None; _ }) -> true
    | (IntLiteral { resolved = Some ty1; _ }, IntLiteral { resolved = Some ty2; _ }) ->
      equal ty1 ty2
    | _ -> false

  let rec hash (ty : t) =
    (* Boost hash combiner *)
    let hash_nums (ns : int list) =
      List.fold_left (fun hash n -> hash lxor (n + 0x9e3779b9 + (hash lsl 6) + (hash asr 2))) 0 ns
    in
    match ty with
    | TVar _ -> failwith "Cannot hash type with any tvars"
    | Any -> 1
    | Unit -> 2
    | Bool -> 3
    | Byte -> 4
    | Int -> 5
    | Long -> 6
    | String -> 7
    | IntLiteral { resolved = None; _ } -> 8
    | IntLiteral { resolved = Some ty; _ } -> hash ty
    | Tuple elements -> hash_nums (9 :: List.map hash elements)
    | Function { params; return } -> hash_nums (10 :: hash return :: List.map hash params)
    | ADT { adt_sig = { id; _ }; tparams } -> hash_nums (11 :: id :: List.map hash tparams)
end

module TypeHashtbl = Hashtbl.Make (TypeHash)
