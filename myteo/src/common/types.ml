open Basic_collections

type tvar_id = int

type t =
  | TVar of tvar_id
  | Unit
  | Bool
  | Int
  | String
  | Function of {
      params: t list;
      return: t;
    }

let max_tvar_id = ref 0

let mk_tvar_id () =
  let tvar_id = !max_tvar_id in
  max_tvar_id := tvar_id + 1;
  tvar_id

let mk_tvar () = TVar (mk_tvar_id ())

let rec get_all_tvars_with_duplicates ty =
  match ty with
  | TVar tvar_id -> [tvar_id]
  | Function { params; return } ->
    List.map get_all_tvars_with_duplicates params
    |> List.concat
    |> List.append (get_all_tvars_with_duplicates return)
  | _ -> []

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

let tvar_occurs_in tvar_id ty = get_all_tvars [ty] |> List.exists (fun t -> t = tvar_id)

let rec pp_with_names ~tvar_to_name ty =
  match ty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | String -> "string"
  | Function { params; return } ->
    (List.map (pp_with_names ~tvar_to_name) params |> String.concat " -> ")
    ^ " -> "
    ^ pp_with_names ~tvar_to_name return
  | TVar tvar_id -> IMap.find tvar_id tvar_to_name

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

let pps ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) tys =
  let all_tvars = get_all_tvars tys in
  let used_names = IMap.fold (fun _ name acc -> SSet.add name acc) tvar_to_name SSet.empty in
  let (tvar_to_name, _) = build_tvar_to_name ~use_tvar_ids (tvar_to_name, used_names) 0 all_tvars in
  List.map (pp_with_names ~tvar_to_name) tys

let pp ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) ty =
  pps ~tvar_to_name ~use_tvar_ids [ty] |> List.hd
