open Basic_collections
open Mir
open Mir_type
open Mir_type_args_hashtbl
open Types

module MirTraitObjectLayout = struct
  type t = {
    trait_sig: TraitSig.t;
    trait_object_agg: Aggregate.t;
    (* Map from method name to its index in the vtable *)
    vtable_indices: int SMap.t;
    instantiations: instantiations;
  }

  and instantiations =
    (* An unparameterized trait object *)
    | Concrete of trait_instance
    (* A generic trait object. Table maps from instantiations of this trait object that have been
       created (aka configurations of type arguments that have been used) to the vtables for those
       type arguments. *)
    | Generic of trait_instance TypeArgsHashtbl.t

  (* Map from type that implements trait to the vtable for that type's trait object *)
  and trait_instance = trait_object_instance TypeArgsHashtbl.t

  and trait_object_instance = {
    vtable: Value.pointer_value;
    agg: Aggregate.t;
  }

  let mk_trait_instance () = TypeArgsHashtbl.create 4

  let mk ~trait_sig ~trait_object_agg ~vtable_indices =
    let instance = mk_trait_instance () in
    let instantiations =
      if TraitSig.is_generic trait_sig then
        Generic (TypeArgsHashtbl.create 4)
      else
        Concrete instance
    in

    { trait_sig; trait_object_agg; vtable_indices; instantiations }

  let add_instantiation instantiations mir_type_args =
    match TypeArgsHashtbl.find_opt instantiations mir_type_args with
    | None ->
      let instance = mk_trait_instance () in
      TypeArgsHashtbl.add instantiations mir_type_args instance;
      instance
    | Some instance -> instance
end

let build_vtable_indices trait_sig =
  SMap.fold
    (fun method_name method_sig (i, indices) ->
      if MethodSig.is_generic method_sig then
        (i, indices)
      else
        (i + 1, SMap.add method_name i indices))
    trait_sig.TraitSig.methods
    (0, SMap.empty)
