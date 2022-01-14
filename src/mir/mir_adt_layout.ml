open Basic_collections
open Mir_type_args_hashtbl
open Mir_type
open Types

type tag_type = Mir_type.Type.t (* Byte or Int *)

type variant_template =
  | TupleTemplate of Type.t list
  | RecordTemplate of (Type.t * Loc.t) SMap.t

module PureEnumLayout = struct
  type t = {
    tags: (* Numeric value of each tag name *) Mir.Value.t SMap.t;
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
 *     { tag, <padding?>, element1, <padding?>, element2 ... <padding?> }
 *
 * All representations have the same size, the size of the largest variant.
 *
 * In the Aggregate, the tag has key "$tag", and all paddings have key "$padding".
 *
 * Variants must place pointer elements in the same location across all variants, so that pointers
 * may be scanned regardless of which variant is present. This is accomplished by grouping all the
 * non-pointer elements together at the beginning of each variant, and grouping all pointer elements
 * together at the end of each variant. Padding is inserted between the non-pointer and pointer
 * elements for each variant to ensure that all pointer elements occupy the same location. Empty
 * locations that contain pointer are marked with "$pointer" elements in both variants and the union
 * aggregate.
 *)
module VariantsTemplate = struct
  type t = {
    (* Tags for all variants, indexed by variant name (numeric values) *)
    tags: Mir.Value.t SMap.t;
    tag_mir_type: tag_type;
    (* Templates for all data variants, indexed by variant name. Templates do not contain any
       padding - padding is calculated during instantiation. *)
    templates: variant_template SMap.t;
    variant_locs: Loc.t SMap.t;
  }
end

module VariantsLayout = struct
  type t = {
    (* Tags for all variants, indexed by variant name (numeric values) *)
    tags: Mir.Value.t SMap.t;
    tag_mir_type: tag_type;
    (* Concrete instance for the union of all variants, (also the instance for enum variants) *)
    union: Aggregate.t;
    (* Total size of union in bytes. Equal to the size of the largest variant layout in the union. *)
    mutable size: int;
    (* Concrete instances of all data variants *)
    mutable variants: Aggregate.t SMap.t;
  }
end

module MirAdtLayout = struct
  type t = {
    adt_sig: AdtSig.t;
    name: string;
    loc: Loc.t;
    template: template;
    layouts: layouts;
  }

  and template =
    | SingleTemplate of variant_template
    | VariantsTemplate of VariantsTemplate.t

  and layouts =
    (* A concrete layout. Lazily fill the value if this type is actually used. *)
    | Concrete of layout option ref
    (* A generic layout. Table maps from instantiations of this ADT that have been created (aka
       configurations of type arguments that have been used) to the value instantiated with those
       type arguments. *)
    | Generic of layout TypeArgsHashtbl.t

  and layout =
    | Aggregate of Aggregate.t
    | Variants of VariantsLayout.t
    | PureEnum of PureEnumLayout.t
    | InlineValue of Mir_type.Type.t
    | ZeroSize
end

let tag_key = "$tag"

let padding_key = "$padding"

let pointer_key = "$pointer"

let mk_tag_element (tag_type : tag_type) : string * Mir_type.Type.t = (tag_key, tag_type)

let mk_padding_element (size : int) : string * Mir_type.Type.t = (padding_key, Array (Byte, size))

let pointer_element = (pointer_key, Mir_type.Type.Pointer Byte)

(* Utilities for aligning and padding variant aggregates *)

let round_up_to_alignment size alignment =
  let overflow = size mod alignment in
  if overflow = 0 then
    size
  else
    size + (alignment - overflow)

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
      | [(key, Array (Byte, size))] when key = padding_key ->
        [mk_padding_element (size + size_to_add)]
      (* Otherwise add new padding element after last element *)
      | [last] -> [last; mk_padding_element size_to_add]
      | hd :: tl -> hd :: add_to_end tl
    in
    add_to_end elements

(* A single element tuple can be inlined as long as it does not contain itself anywhere in the type
   of its single element. Must recurse through other inlined single element tuples as it may be cyclic. *)
let rec can_inline_single_element_tuple (root_adt_sig : Types.AdtSig.t) (element_ty : Types.Type.t)
    =
  let check_types = List.for_all (can_inline_single_element_tuple root_adt_sig) in
  match element_ty with
  | Types.Type.ADT { adt_sig; _ } when adt_sig.id = root_adt_sig.id -> false
  | ADT { type_args; adt_sig } ->
    check_types type_args
    &&
    (* Recurse into other inlined non-variant single element tuples *)
    if SMap.cardinal adt_sig.variants = 1 then
      match SMap.choose adt_sig.variants with
      | (_, { kind = Tuple [single_element]; _ }) ->
        can_inline_single_element_tuple root_adt_sig single_element
      | _ -> true
    else
      true
  | Tuple elements -> check_types elements
  | _ -> true
