open Asm
open Basic_collections
open Mir_type

module AggregateElement = struct
  type t = {
    (* Offset of this element from the beginning of the aggregate *)
    offset: int;
    (* Total size of the element *)
    size: int;
  }
end

module AggregateLayout = struct
  type t = {
    agg: Aggregate.t;
    size: int;
    alignment: int;
    elements: AggregateElement.t array;
  }

  let get_element agg_layout i = agg_layout.elements.(i)
end

let rec align_of_data_value d =
  match d with
  | ImmediateData imm -> bytes_of_size (size_of_immediate imm)
  | AsciiData _ -> 1
  | LabelData _ -> 8
  | SSELiteral _ -> 16
  | ArrayData data ->
    List.fold_left
      (fun max_align value ->
        let align = align_of_data_value value in
        if align > max_align then
          align
        else
          max_align)
      1
      data

let align_to_data_section_align_index align =
  match align with
  | 1 -> 0
  | 2 -> 1
  | 4 -> 2
  | 8 -> 3
  | 16 -> 4
  | _ -> failwith "Invalid alignment"

module AggregateLayoutCache = struct
  type t = {
    (* Map from aggregate id to the layout of that aggregate *)
    mutable cache: AggregateLayout.t IMap.t;
  }

  let mk () : t = { cache = IMap.empty }

  let get (cache : t) (agg : Aggregate.t) = IMap.find agg.id cache.cache

  let add (cache : t) (agg : Aggregate.t) (layout : AggregateLayout.t) =
    cache.cache <- IMap.add agg.id layout cache.cache
end

let rec size_of_mir_type ~agg_cache mir_type =
  match mir_type with
  | Type.Bool
  | Byte ->
    1
  | Short -> 2
  | Int -> 4
  | Long
  | Double
  | Function
  | Pointer _ ->
    8
  | Aggregate agg ->
    let agg_layout = AggregateLayoutCache.get agg_cache agg in
    agg_layout.size
  | Array (ty, size) -> size_of_mir_type ~agg_cache ty * size

let rec alignment_of_mir_type ~agg_cache mir_type =
  match mir_type with
  | Type.Aggregate agg ->
    let agg_layout = AggregateLayoutCache.get agg_cache agg in
    agg_layout.alignment
  | Array (ty, _) -> alignment_of_mir_type ~agg_cache ty
  | _ -> size_of_mir_type ~agg_cache mir_type

let add_agg_layout ~agg_cache agg =
  let open Aggregate in
  let current_offset = ref 0 in
  let largest_alignment = ref 0 in

  (* Calculate offsets for each aggregate element *)
  let elements =
    List.map
      (fun (_, ty) ->
        let size = size_of_mir_type ~agg_cache ty in
        let alignment = alignment_of_mir_type ~agg_cache ty in
        largest_alignment := max alignment !largest_alignment;

        (* Add padding to align element *)
        let align_overflow = !current_offset mod alignment in
        if align_overflow <> 0 then current_offset := !current_offset + (size - align_overflow);

        let offset = !current_offset in
        current_offset := offset + size;
        { AggregateElement.offset; size })
      agg.elements
  in

  (* Aggregate has alignment of its largest element. Aggregate size must be multiple of alignment,
     so insert padding to end of aggregate if necessary. *)
  let alignment = !largest_alignment in
  let align_overflow =
    if alignment = 0 then
      0
    else
      !current_offset mod alignment
  in
  if align_overflow <> 0 then current_offset := !current_offset + (alignment - align_overflow);

  let agg_layout =
    { AggregateLayout.agg; size = !current_offset; alignment; elements = Array.of_list elements }
  in
  AggregateLayoutCache.add agg_cache agg agg_layout
