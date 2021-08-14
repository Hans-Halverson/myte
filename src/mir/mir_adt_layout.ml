open Basic_collections
open Mir_type
open Types

type tag_type =
  [ `ByteT
  | `IntT
  ]

type template =
  | TupleTemplate of Type.t list
  | RecordTemplate of (Type.t * Loc.t) SMap.t

type 'a instantiations =
  (* A concrete layout. Lazy fill the value if this type is actually used. *)
  | Concrete of 'a option ref
  (* A generic layout. Table maps from instantiations of this ADT that have been created (aka
     configurations of type arguments that have been used) to the value instantiated with those
     type arguments. *)
  | Generic of 'a TypeArgsHashtbl.t

module MirAdtAggregateLayout = struct
  type t = {
    template: template;
    instantiations: Aggregate.t instantiations;
  }
end

module MirAdtPureEnumLayout = struct
  type t = {
    tags: Mir.Value.t SMap.t;
    tag_mir_type: tag_type;
  }
end

(* Information needed to create values for variants that contain data. Every enum and data variant
 * is assigned a unique integer tag (of the smallest possible integer type).
 *
 * When referring to the entire value when the exact variant is unknown, or when referring to an
 * enum variant, the value is represented as an aggregate containing the tag at the beginning,
 * followed by a byte array of padding to pad it to the size of its largest variant.
 *     { tag, <padding?> }
 *
 * When referring to a specific data variant, the value is represented as an aggregate containing
 * the tag at the beginning, followed by elements of that variant if it contains data. Padding
 * (represented as byte array elements in the aggregate) is inserted between the tag and data
 * elements to satisfy alignment requirements, and then after all elements to pad to the size of
 * the largest variant.
 *     { tag, <padding?>, element1, <padding?> , element2 ... <padding?> }
 *
 * All representations have the same size, the size of the largest variant.
 *
 * In the Aggregate, the tag has key "$tag", and all paddings have key "$padding".
 *)
module MirAdtVariantsLayout = struct
  type t = {
    (* Tags for all variants, indexed by variant name *)
    tags: Mir.Value.numeric_value SMap.t;
    tag_mir_type: tag_type;
    (* Templates for all data variants, indexed by variant name. Templates do not contain any
       padding - padding is calculated during instantiation. *)
    templates: template SMap.t;
    instantiations: instance instantiations;
    variant_locs: Loc.t SMap.t;
  }

  and instance = {
    (* Total size of union in bytes. Equal to the size of the largest variant layout in the union. *)
    mutable size: int;
    (* Concrete instance for the union of all variants, (also the instance for enum variants) *)
    union: Aggregate.t;
    (* Concrete instances of all data variants *)
    mutable variants: Aggregate.t SMap.t;
  }
end

module MirAdtLayout = struct
  type t = {
    adt_sig: AdtSig.t;
    name: string;
    loc: Loc.t;
    layout: layout;
  }

  and layout =
    | Aggregate of MirAdtAggregateLayout.t
    | Variants of MirAdtVariantsLayout.t
    | PureEnum of MirAdtPureEnumLayout.t
    | InlineValue of Type.t
end

let tag_key = "$tag"

let padding_key = "$padding"

let mk_tag_element (tag_type : tag_type) : string * Mir_type.Type.t =
  (tag_key, (tag_type :> Mir_type.Type.t))

let mk_padding_element (size : int) : string * Mir_type.Type.t =
  (padding_key, `ArrayT (`ByteT, size))

(* Utilities for aligning and padding variant aggregates *)

let rec size_of_type mir_type =
  match mir_type with
  | `UnitT
  | `BoolT
  | `ByteT ->
    1
  | `IntT -> 4
  | `LongT
  | `FunctionT
  | `PointerT _ ->
    8
  | `ArrayT (mir_type, size) -> size * size_of_type mir_type
  | `AggregateT _ -> failwith "Aggregates not allowed as top level value of other aggregates"

let alignment_of_type mir_type =
  match mir_type with
  | `ArrayT (mir_type, _) -> size_of_type mir_type
  | _ -> size_of_type mir_type

(* Add padding and after aggregate elements to satisfy alignment requirements. Return the new
   aggregate elements with padding inserted, and the total size of the padded aggregate. *)
let align_and_pad_aggregate_elements elements =
  let current_offset = ref 0 in
  let largest_alignment = ref 0 in

  (* Adding padding if necessary before each aggregate element *)
  let elements =
    List.map
      (fun ((_, mir_type) as element) ->
        let size = size_of_type mir_type in
        let alignment = alignment_of_type mir_type in
        largest_alignment := max alignment !largest_alignment;

        (* Add padding to align element *)
        let align_overflow = !current_offset mod alignment in
        let padded_element =
          if align_overflow <> 0 then (
            let padding_size = size - align_overflow in
            current_offset := !current_offset + padding_size;
            [mk_padding_element padding_size; element]
          ) else
            [element]
        in

        let offset = !current_offset in
        current_offset := offset + size;
        padded_element)
      elements
  in
  let elements = List.flatten elements in

  (* Aggregate has alignment of its largest element. Aggregate size must be multiple of alignment,
     so insert padding to end of aggregate if necessary. *)
  let alignment = !largest_alignment in
  let align_overflow = !current_offset mod alignment in
  let elements =
    if align_overflow <> 0 then (
      let padding_size = alignment - align_overflow in
      current_offset := !current_offset + padding_size;
      elements @ [mk_padding_element padding_size]
    ) else
      elements
  in

  (elements, !current_offset)

(* Add padding to the end of aggregate elements of a given size, to match a target size *)
let add_end_padding elements current_size target_size =
  if current_size = target_size then
    elements
  else
    let size_to_add = target_size - current_size in
    let rec add_to_end (elements : (string * Mir_type.Type.t) list) =
      match elements with
      | [] -> failwith "Expected nonempty list"
      (* If last element is already padding, extend existing padding *)
      | [(key, `ArrayT (`ByteT, size))] when key = padding_key ->
        [mk_padding_element (size + size_to_add)]
      (* Otherwise add new padding element after last element *)
      | [last] -> [last; mk_padding_element size_to_add]
      | hd :: tl -> hd :: add_to_end tl
    in
    add_to_end elements
