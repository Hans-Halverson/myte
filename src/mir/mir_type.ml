open Basic_collections

type aggregate_id = int

module rec Type : sig
  type t =
    [ `UnitT
    | `BoolT
    | `IntT
    | `ByteT
    | `LongT
    | `FunctionT
    | `PointerT of t
    | `AggregateT of Aggregate.t
    | `ArrayT of t * int
    ]

  and numeric_type =
    [ `ByteT
    | `IntT
    | `LongT
    ]
end =
  Type

and Aggregate : sig
  type t = {
    id: aggregate_id;
    name: string;
    loc: Loc.t;
    (* Elements along with their name. For records this is the field name, for tuples this is a
       string of the tuple index. Also may be special cases `$tag` or `$padding`. *)
    mutable elements: (string * Type.t) list;
  }
end =
  Aggregate

let max_aggregate_id = ref 0

let mk_aggregate_id () =
  let agg_id = !max_aggregate_id in
  max_aggregate_id := agg_id + 1;
  agg_id

(* Look up an element by name in an aggregate type, throwing if no element with that name is found.
   Return a tuple of the element's type and its index in the aggregate. *)
let lookup_element agg name =
  let open Aggregate in
  let rec inner elements i =
    match elements with
    | [] -> failwith "Field not defined for aggregate"
    | (element_name, element_ty) :: _ when element_name = name -> (element_ty, i)
    | _ :: tl -> inner tl (i + 1)
  in
  inner agg.elements 0

(* Tuple indices are converted to strings in aggregate element list. Keep cache of indices converted
   to keys to avoid a large number of calls to `string_of_int`. *)
module TupleKeyCache = struct
  type t = {
    mutable index_to_key: string IMap.t;
    mutable key_to_index: int SMap.t;
  }

  let cache = { index_to_key = IMap.empty; key_to_index = SMap.empty }

  (* Get key from index, adding to cache if it does not yet exist *)
  let get_key index =
    match IMap.find_opt index cache.index_to_key with
    | Some key -> key
    | None ->
      let key = string_of_int index in
      cache.index_to_key <- IMap.add index key cache.index_to_key;
      cache.key_to_index <- SMap.add key index cache.key_to_index;
      key

  (* Get index from key, key must already exist in cache (as keys can only be generated by get_key) *)
  let get_index key = SMap.find key cache.key_to_index
end

let rec type_to_string ty =
  match ty with
  | `UnitT -> "unit"
  | `ByteT -> "byte"
  | `IntT -> "int"
  | `LongT -> "long"
  | `BoolT -> "bool"
  | `FunctionT -> "fn"
  | `PointerT ty -> type_to_string ty ^ "*"
  | `AggregateT { Aggregate.name; _ } -> name
  | `ArrayT (ty, size) -> Printf.sprintf "%s[%d]" (type_to_string ty) size

let cast_to_pointer_type ty =
  match ty with
  | `PointerT _ as ty -> ty
  | _ -> failwith "Expected pointer type"

let cast_to_aggregate_type ty =
  match ty with
  | `AggregateT _ as ty -> ty
  | _ -> failwith "Expected aggregate type"

module TypeArgs = struct
  type t = Type.t list

  let equal (tys1 : t) (tys2 : t) =
    let rec types_equal ty1 ty2 =
      match (ty1, ty2) with
      | (`UnitT, `UnitT)
      | (`BoolT, `BoolT)
      | (`IntT, `IntT)
      | (`ByteT, `ByteT)
      | (`LongT, `LongT)
      | (`FunctionT, `FunctionT) ->
        true
      | (`PointerT ty1, `PointerT ty2) -> types_equal ty1 ty2
      | (`AggregateT agg1, `AggregateT agg2) -> Aggregate.(agg1.id = agg2.id)
      | (`ArrayT (ty1, size1), `ArrayT (ty2, size2)) -> size1 = size2 && types_equal ty1 ty2
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
      | `IntT -> 2
      | `ByteT -> 3
      | `LongT -> 4
      | `FunctionT -> 5
      | `PointerT ty -> hash_nums [6; hash ty]
      | `AggregateT { Aggregate.id; _ } -> hash_nums [7; id]
      | `ArrayT (ty, size) -> hash_nums [8; hash ty; size]
    in
    hash_nums (List.map hash tys)

  let to_string (tys : t) =
    if tys = [] then
      ""
    else
      let ty_strings = List.map type_to_string tys in
      "<" ^ String.concat "," ty_strings ^ ">"
end

module TypeArgsHashtbl = Hashtbl.Make (TypeArgs)
