open Basic_collections

module TParam = struct
  type id = int

  type t = {
    id: id;
    name: string;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let mk name = { id = mk_id (); name }
end

type tvar_id = int

type adt_sig_id = int

type t =
  | TVar of tvar_id
  | TParam of TParam.t
  | Any
  | Unit
  | Bool
  | Byte
  | Int
  | Long
  | IntLiteral of int_literal
  | String
  | Array of t
  | Tuple of t list
  | Function of {
      tparams: TParam.t list;
      params: t list;
      return: t;
    }
  | ADT of {
      adt_sig: adt_sig;
      type_args: t list;
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
  mutable type_params: TParam.t list;
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

let mk_adt_sig name type_params =
  { id = mk_adt_sig_id (); name; type_params; variant_sigs = SMap.empty }

let get_all_tvars_with_duplicates ty =
  let rec inner acc ty =
    match ty with
    | TVar tvar_id -> tvar_id :: acc
    | Tuple elements -> List.fold_left inner acc elements
    | Array element -> inner acc element
    | Function { tparams = _; params; return } ->
      let acc = List.fold_left inner acc params in
      inner acc return
    | ADT { type_args; _ } -> List.fold_left inner acc type_args
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

let rec substitute_type_params type_params ty =
  match ty with
  | Any
  | Unit
  | Bool
  | Byte
  | Int
  | Long
  | String
  | TVar _
  | IntLiteral { resolved = None; _ } ->
    ty
  | IntLiteral { resolved = Some resolved; _ } -> substitute_type_params type_params resolved
  | Array element -> Array (substitute_type_params type_params element)
  | Tuple elements -> Tuple (List.map (substitute_type_params type_params) elements)
  | Function { tparams = func_type_params; params; return } ->
    let params' = List.map (substitute_type_params type_params) params in
    let return' = substitute_type_params type_params return in
    Function { tparams = func_type_params; params = params'; return = return' }
  | ADT { adt_sig; type_args } ->
    ADT { adt_sig; type_args = List.map (substitute_type_params type_params) type_args }
  | TParam { TParam.id; name = _ } ->
    (match IMap.find_opt id type_params with
    | None -> ty
    | Some ty -> substitute_type_params type_params ty)

(* Generate a new set of type_params for this ADT declaration type *)
let refresh_adt_type_params ty =
  match ty with
  | ADT { adt_sig = { type_params; _ } as adt_sig; _ } ->
    let fresh_type_args = List.map (fun _ -> mk_tvar ()) type_params in
    ADT { adt_sig; type_args = fresh_type_args }
  | _ -> failwith "Expected ADT"

(* Generate map of type params bound to type args to be used for type param substitution. *)
let bind_type_params_to_args type_params type_args =
  let type_params_and_args = List.combine type_params type_args in
  List.fold_left
    (fun map (type_param, ty) -> IMap.add type_param.TParam.id ty map)
    IMap.empty
    type_params_and_args

let get_adt_tparam_bindings ty =
  match ty with
  | ADT { adt_sig = { type_params; _ }; type_args } ->
    bind_type_params_to_args type_params type_args
  | _ -> failwith "Expected ADT"

let concat_and_wrap (pre, post) elements = pre ^ String.concat ", " elements ^ post

let rec pp_with_names ~tvar_to_name ty =
  let pp_tparams tparams =
    if tparams = [] then
      ""
    else
      let pp_tparams = List.map (fun tparam -> tparam.TParam.name) tparams in
      concat_and_wrap ("<", ">") pp_tparams
  in
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
  | Array element -> "Array<" ^ pp_with_names ~tvar_to_name element ^ ">"
  | Tuple elements ->
    let element_names = List.map (pp_with_names ~tvar_to_name) elements in
    concat_and_wrap ("(", ")") element_names
  | Function { tparams; params; return } ->
    let pp_function_part ty =
      let pp_param = pp_with_names ~tvar_to_name ty in
      match ty with
      | Function _ -> "(" ^ pp_param ^ ")"
      | _ -> pp_param
    in
    let pp_tparams = pp_tparams tparams in
    let pp_params =
      match params with
      | [param] -> pp_function_part param
      | _ ->
        let pp_params = List.map (fun param -> pp_with_names ~tvar_to_name param) params in
        concat_and_wrap ("(", ")") pp_params
    in
    pp_tparams ^ pp_params ^ " -> " ^ pp_function_part return
  | ADT { adt_sig = { name; _ }; type_args } ->
    if type_args = [] then
      name
    else
      let pp_args = List.map (pp_with_names ~tvar_to_name) type_args in
      concat_and_wrap (name ^ "<", ">") pp_args
  | TVar tvar_id ->
    let x = IMap.find tvar_id tvar_to_name in
    x
  | TParam { TParam.name; id = _ } -> name

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

let get_adt_sig adt =
  match adt with
  | ADT { adt_sig; _ } -> adt_sig
  | _ -> failwith (Printf.sprintf "Expected ADT %s\n" (pp adt))

let get_tuple_variant adt_sig name =
  match SMap.find name adt_sig.variant_sigs with
  | TupleVariantSig element_sigs -> element_sigs
  | _ -> failwith "Expected TupleVariantSig"

let get_record_variant adt_sig name =
  match SMap.find name adt_sig.variant_sigs with
  | RecordVariantSig field_sigs -> field_sigs
  | _ -> failwith "Expected RecordVariantSig"

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
    | (TParam { id = id1; name = _ }, TParam { id = id2; name = _ }) -> id1 = id2
    | (Array element1, Array element2) -> equal element1 element2
    | (Tuple elements1, Tuple elements2) ->
      List.length elements1 = List.length elements2 && List.for_all2 equal elements1 elements2
    | ( Function { tparams = tparams1; params = params1; return = return1 },
        Function { tparams = tparams2; params = params2; return = return2 } ) ->
      List.length tparams1 = List.length tparams2
      && List.for_all2 (fun t1 t2 -> t1.TParam.name = t2.TParam.name) tparams1 tparams2
      && List.length params1 = List.length params2
      && List.for_all2 equal params1 params2
      && equal return1 return2
    | (ADT { adt_sig = adt_sig1; type_args = args1 }, ADT { adt_sig = adt_sig2; type_args = args2 })
      ->
      adt_sig1 == adt_sig2 && List.for_all2 equal args1 args2
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
    | TParam { TParam.id; name = _ } -> hash_nums [1; id]
    | Any -> 2
    | Unit -> 3
    | Bool -> 4
    | Byte -> 5
    | Int -> 6
    | Long -> 7
    | String -> 8
    | IntLiteral { resolved = None; _ } -> 9
    | IntLiteral { resolved = Some ty; _ } -> hash ty
    | Array element -> hash_nums [10; hash element]
    | Tuple elements -> hash_nums (11 :: List.map hash elements)
    | Function { tparams; params; return } ->
      hash_nums
        ((12 :: hash return :: List.map (fun tp -> tp.TParam.id) tparams) @ List.map hash params)
    | ADT { adt_sig = { id; _ }; type_args } -> hash_nums (13 :: id :: List.map hash type_args)
end

module TypeHashtbl = Hashtbl.Make (TypeHash)
