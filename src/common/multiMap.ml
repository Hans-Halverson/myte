module Make (KeyType : Map.OrderedType) (ValueType : Set.OrderedType) = struct
  module KMap = Map.Make (KeyType)
  module VSet = Set.Make (ValueType)

  type t = VSet.t KMap.t

  let empty = KMap.empty

  let is_empty mmap = KMap.is_empty mmap

  let add k v mmap =
    KMap.add
      k
      (match KMap.find_opt k mmap with
      | None -> VSet.singleton v
      | Some vs -> VSet.add v vs)
      mmap

  let remove k v mmap =
    match KMap.find_opt k mmap with
    | None -> mmap
    | Some vs ->
      let vs' = VSet.remove v vs in
      if VSet.is_empty vs' then
        KMap.remove k mmap
      else
        KMap.add k vs' mmap

  let remove_key k mmap = KMap.remove k mmap

  let contains k v mmap =
    match KMap.find_opt k mmap with
    | None -> false
    | Some vs -> VSet.mem v vs

  let contains_key k mmap = KMap.mem k mmap

  let find_all k mmap =
    match KMap.find_opt k mmap with
    | None -> VSet.empty
    | Some vs -> vs

  let choose mmap =
    let (k, vs) = KMap.choose mmap in
    (k, VSet.choose vs)
end
