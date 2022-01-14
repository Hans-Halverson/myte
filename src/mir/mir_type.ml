open Basic_collections

type aggregate_id = int

module rec Type : sig
  type t =
    | Bool
    | Byte
    | Int
    | Long
    | Function
    | Pointer of t
    | Aggregate of Aggregate.t
    | Array of t * int
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

let zero_size_name = "_ZeroSize"

let lookup_element_opt agg name =
  let open Aggregate in
  let rec inner elements i =
    match elements with
    | [] -> None
    | (element_name, element_ty) :: _ when element_name = name -> Some (element_ty, i)
    | _ :: tl -> inner tl (i + 1)
  in
  inner agg.elements 0

(* Look up an element by name in an aggregate type, throwing if no element with that name is found.
   Return a tuple of the element's type and its index in the aggregate. *)
let lookup_element agg name =
  match lookup_element_opt agg name with
  | Some result -> result
  | None -> failwith "Field not defined for aggregate"

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

(* Arbitrarily choose return type to use when there is no return type for an instruction *)
let no_return_type = Type.Bool

let rec types_equal (ty1 : Type.t) (ty2 : Type.t) : bool =
  match (ty1, ty2) with
  | (Bool, Bool)
  | (Byte, Byte)
  | (Int, Int)
  | (Long, Long)
  | (Function, Function) ->
    true
  | (Pointer ty1, Pointer ty2) -> types_equal ty1 ty2
  | (Aggregate agg1, Aggregate agg2) -> agg1.id = agg2.id
  | (Array (ty1, size1), Array (ty2, size2)) -> types_equal ty1 ty2 && size1 = size2
  | _ -> false

let rec type_to_string (ty : Type.t) =
  match ty with
  | Byte -> "byte"
  | Int -> "int"
  | Long -> "long"
  | Bool -> "bool"
  | Function -> "fn"
  | Pointer ty -> type_to_string ty ^ "*"
  | Aggregate { Aggregate.name; _ } -> name
  | Array (ty, size) -> Printf.sprintf "%s[%d]" (type_to_string ty) size

let byte_size = 1

let int_size = 4

let ptr_size = 8

let rec size_of_type (mir_type : Type.t) =
  match mir_type with
  | Bool
  | Byte ->
    byte_size
  | Int -> int_size
  | Long
  | Function
  | Pointer _ ->
    ptr_size
  | Array (mir_type, size) -> size * size_of_type mir_type
  | Aggregate _ -> failwith "Aggregates not allowed as top level value of other aggregates"

let alignment_of_type (mir_type : Type.t) =
  match mir_type with
  | Array (mir_type, _) -> size_of_type mir_type
  | _ -> size_of_type mir_type

let cast_to_pointer_type (ty : Type.t) : Type.t =
  match ty with
  | Pointer element_type -> element_type
  | _ -> failwith "Expected pointer type"

let cast_to_aggregate_type (ty : Type.t) : Aggregate.t =
  match ty with
  | Aggregate agg -> agg
  | _ -> failwith "Expected aggregate type"

let is_pointer_type (ty : Type.t) =
  match ty with
  | Pointer _ -> true
  | _ -> false

let is_numeric_type (v : Type.t) : bool =
  match v with
  | Bool
  | Byte
  | Int
  | Long ->
    true
  | _ -> false
