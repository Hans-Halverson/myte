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
    tag_mir_type: tag_type;
    tags: Mir.cf_value SMap.t;
  }
end

(* Information needed to create values for variants that contain data. Every enum and data variant
 * is assigned a unique integer tag (of the smallest possible integer type).
 *
 * When referring to the entire value when the exact variant is unknown, the value is represented
 * as an aggregate containing the tag at the beginning, followed by a byte array of padding to pad
 * it to the size of its largest variant.
 *     { tag, <padding?> }
 *
 * When referring to a specific variant, the value is represented as an aggregate containing the
 * tag at the beginning, followed by elements of that variant if it contains data. Padding
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
    tag_mir_type: tag_type;
    (* Tags for all variants (enum and data), indexed by variant name *)
    tags: Mir.cf_value SMap.t;
    (* Templates for all data variants, indexed by variant name. Templates do not contain any
       padding - padding is calculated during instantiation. *)
    templates: template SMap.t;
    instantiations: instance instantiations;
  }

  and instance = {
    (* Total size of union in bytes. Equal to the size of the largest variant layout in the union. *)
    size: int;
    (* Concrete instances of all data variants *)
    variants: Aggregate.t SMap.t;
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
