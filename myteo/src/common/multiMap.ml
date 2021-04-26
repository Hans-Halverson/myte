module Make (KeyType : Map.OrderedType) (ValueType : Set.OrderedType) = struct
  module KMap = Map.Make (KeyType)
  module VSet = Set.Make (ValueType)

  type t = VSet.t KMap.t

  let empty = KMap.empty

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
    | Some vs -> KMap.add k (VSet.remove v vs) mmap

  let remove_key k mmap = KMap.remove k mmap

  let contains k v mmap =
    match KMap.find_opt k mmap with
    | None -> false
    | Some vs -> VSet.mem v vs

  let find_all k mmap =
    match KMap.find_opt k mmap with
    | None -> VSet.empty
    | Some vs -> vs
end
