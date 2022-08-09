open Basic_collections

type id = int

type t = {
  id: id;
  name: string list;
}

let max_id = ref 0

let mk_id () =
  let id = !max_id in
  max_id := id + 1;
  id

let mk ~name = { id = mk_id (); name }

let none = { id = 0; name = [] }

let module_by_loc : t LocMap.t ref = ref LocMap.empty

let set_module_for_module_loc loc module_ = module_by_loc := LocMap.add loc module_ !module_by_loc

let get_module_for_module_loc loc = LocMap.find loc !module_by_loc

let equal m1 m2 = m1.id == m2.id

let in_stdlib module_ =
  match module_.name with
  | "std" :: _ -> true
  | _ -> false
