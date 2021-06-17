type aggregate_id = int

module rec Type : sig
  type t =
    [ `UnitT
    | `BoolT
    | `StringT
    | `IntT
    | `ByteT
    | `LongT
    | `FunctionT
    | `PointerT of t
    | `AggregateT of Aggregate.t
    ]
end =
  Type

and Aggregate : sig
  type t = {
    id: aggregate_id;
    name: string;
    loc: Loc.t;
    (* Elements along with their optional name *)
    mutable elements: (string option * Type.t) list;
  }
end =
  Aggregate

let max_aggregate_id = ref 0

let mk_aggregate_id () =
  let agg_id = !max_aggregate_id in
  max_aggregate_id := agg_id + 1;
  agg_id

let mk_aggregate name loc elements = { Aggregate.id = mk_aggregate_id (); name; loc; elements }

let rec type_to_string ty =
  match ty with
  | `UnitT -> "unit"
  | `ByteT -> "byte"
  | `IntT -> "int"
  | `LongT -> "long"
  | `BoolT -> "bool"
  | `StringT -> "string"
  | `FunctionT -> "fn"
  | `PointerT ty -> type_to_string ty ^ "*"
  | `AggregateT { Aggregate.name; _ } -> name

module TypeArgs = struct
  type t = Type.t list

  let equal (tys1 : t) (tys2 : t) =
    let rec types_equal ty1 ty2 =
      match (ty1, ty2) with
      | (`UnitT, `UnitT)
      | (`BoolT, `BoolT)
      | (`StringT, `StringT)
      | (`IntT, `IntT)
      | (`ByteT, `ByteT)
      | (`LongT, `LongT)
      | (`FunctionT, `FunctionT) ->
        true
      | (`PointerT ty1, `PointerT ty2) -> types_equal ty1 ty2
      | (`AggregateT agg1, `AggregateT agg2) -> Aggregate.(agg1.id = agg2.id)
      | _ -> false
    in
    List.length tys1 = List.length tys2 && List.for_all2 types_equal tys1 tys2

  let hash (tys : t) =
    (* Boost hash combiner *)
    let hash_nums (ns : int list) =
      List.fold_left (fun hash n -> hash lxor (n + 0x9e3779b9 + (hash lsl 6) + (hash asr 2))) 0 ns
    in
    let rec hash ty =
      match ty with
      | `UnitT -> 0
      | `BoolT -> 1
      | `StringT -> 2
      | `IntT -> 3
      | `ByteT -> 4
      | `LongT -> 5
      | `FunctionT -> 6
      | `PointerT ty -> hash_nums [7; hash ty]
      | `AggregateT { Aggregate.id; _ } -> hash_nums [8; id]
    in
    hash_nums (List.map hash tys)

  let to_string (tys : t) =
    let ty_strings = List.map type_to_string tys in
    String.concat "," ty_strings
end

module TypeArgsHashtbl = Hashtbl.Make (TypeArgs)
