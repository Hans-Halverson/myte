open Basic_collections
open Mir_type
open Types

(* TODO: Add LayoutInlineValue *)
(* TODO: Add LayoutPureEnum *)
(* TODO: Add LayoutVariantUnion *)

module MirAdtAggregateLayout = struct
  type t = {
    template: template;
    instantiations: instantiations;
  }

  and template =
    | TupleTemplate of Type.t list
    | RecordTemplate of (Type.t * Loc.t) SMap.t

  and instantiations =
    (* A concrete aggregate layout. Lazy fill the aggregate if this type is actually used. *)
    | Concrete of Aggregate.t option ref
    (* A generic aggregate layout. Table maps from instantations of this ADT that have been created
       (aka configurations of type arguments that have been used) to the aggregate instantiated
       with those type arguments. *)
    | Generic of Aggregate.t TypeArgsHashtbl.t
end

module MirAdtLayout = struct
  type t = {
    adt_sig: AdtSig.t;
    name: string;
    loc: Loc.t;
    layout: layout;
  }

  and layout = Aggregate of MirAdtAggregateLayout.t
end
