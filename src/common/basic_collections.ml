module type SORTED = sig
  type t
  val compare : t -> t -> int
end

module MakeCollection (S : SORTED) = struct
  type t = S.t
  let compare = S.compare
  module Set = Set.Make (S)
  module Map = Map.Make (S)
end

module IntCollection = MakeCollection (Int)
module Int64Collection = MakeCollection (Int64)
module BoolCollection = MakeCollection (Bool)
module StringCollection = MakeCollection (String)

module ISet = IntCollection.Set
module IMap = IntCollection.Map
module IIMMap = MultiMap.Make (IMap) (ISet)
module I64Set = Int64Collection.Set
module I64Map = Int64Collection.Map
module SSet = StringCollection.Set
module SMap = StringCollection.Map
module BSet = BoolCollection.Set
module BMap = BoolCollection.Map
module LocSet = Loc.Set
module LocMap = Loc.Map

let string_of_iset iset =
  let elements = ISet.to_seq iset |> List.of_seq |> List.map string_of_int |> String.concat ", " in
  "(" ^ elements ^ ")"
