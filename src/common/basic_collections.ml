module ISet = Set.Make (Int)
module IMap = Map.Make (Int)
module IIMMap = MultiMap.Make (Int) (Int)
module I64Set = Set.Make (Int64)
module SSet = Set.Make (String)
module SMap = Map.Make (String)
module BSet = Set.Make (Bool)
module LocSet = Set.Make (Loc)
module LocMap = Map.Make (Loc)

let string_of_iset iset =
  let elements = ISet.to_seq iset |> List.of_seq |> List.map string_of_int |> String.concat ", " in
  "(" ^ elements ^ ")"
