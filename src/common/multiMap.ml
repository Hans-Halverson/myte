module type S = sig
  type key

  type value

  module KMap : Map.S with type key = key

  module VSet : Set.S with type elt = value

  type t = VSet.t KMap.t

  val empty : t

  val is_empty : t -> bool

  val add : key -> value -> t -> t

  val remove : key -> value -> t -> t

  val remove_key : key -> t -> t

  val contains : key -> value -> t -> bool

  val contains_key : key -> t -> bool

  val find_all : key -> t -> VSet.t

  val choose : t -> key * value

  val iter : (key -> VSet.t -> unit) -> t -> unit
end

module type KEY_TYPE = sig
  type t

  module Map : Map.S with type key = t

  val compare : t -> t -> int
end

module type VALUE_TYPE = sig
  type t

  module Set : Set.S with type elt = t

  val compare : t -> t -> int
end

module type KEY_AND_VALUE_TYPE = sig
  type t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  val compare : t -> t -> int
end

module Make (Key : KEY_TYPE) (Value : VALUE_TYPE) = struct
  module KMap = Key.Map

  module VSet = Value.Set

  type t = Value.Set.t Key.Map.t

  type key = Key.t

  type value = Value.t

  let empty = Key.Map.empty

  let is_empty mmap = Key.Map.is_empty mmap

  let add k v mmap =
    Key.Map.add
      k
      (match Key.Map.find_opt k mmap with
      | None -> Value.Set.singleton v
      | Some vs -> Value.Set.add v vs)
      mmap

  let remove k v mmap =
    match Key.Map.find_opt k mmap with
    | None -> mmap
    | Some vs ->
      let vs' = Value.Set.remove v vs in
      if Value.Set.is_empty vs' then
        Key.Map.remove k mmap
      else
        Key.Map.add k vs' mmap

  let remove_key k mmap = Key.Map.remove k mmap

  let contains k v mmap =
    match Key.Map.find_opt k mmap with
    | None -> false
    | Some vs -> Value.Set.mem v vs

  let contains_key k mmap = Key.Map.mem k mmap

  let find_all k mmap =
    match Key.Map.find_opt k mmap with
    | None -> Value.Set.empty
    | Some vs -> vs

  let choose mmap =
    let (k, vs) = Key.Map.choose mmap in
    (k, Value.Set.choose vs)

  let iter = Key.Map.iter
end
