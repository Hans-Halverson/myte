open Basic_collections
open Mir
open Mir_type
open Types

module MirTraitObjectVtable = struct
  type t = {
    trait_sig: TraitSig.t;
    (* Map from method name to its index in the vtable *)
    vtable_indices: int SMap.t;
    vtable_size: int;
    instantiations: instantiations;
  }

  and instantiations =
    (* An unparameterized trait object *)
    | Concrete of type_vtables
    (* A generic trait object. Table maps from instantiations of this trait object that have been
       created (aka configurations of type arguments that have been used) to the vtables for those
       type arguments. *)
    | Generic of type_vtables TypeArgsHashtbl.t

  (* Map from type that implements trait to the vtable for that type's trait object *)
  and type_vtables = Value.pointer_value TypeArgsHashtbl.t

  let mk_type_vtables () = TypeArgsHashtbl.create 4

  let mk trait_sig =
    (* Gather all non-generic methods into vtable in lexicographic order *)
    let (vtable_size, vtable_indices) =
      SMap.fold
        (fun method_name method_sig (i, indices) ->
          if MethodSig.is_generic method_sig then
            (i, indices)
          else
            (i + 1, SMap.add method_name i indices))
        trait_sig.TraitSig.methods
        (0, SMap.empty)
    in
    let vtables = mk_type_vtables () in
    let instantiations =
      if trait_sig.type_params = [] then
        Concrete vtables
      else
        Generic (TypeArgsHashtbl.create 4)
    in
    { trait_sig; vtable_indices; vtable_size; instantiations }

  let add_instantiation instantiations mir_type_args =
    match TypeArgsHashtbl.find_opt instantiations mir_type_args with
    | None ->
      let vtables = mk_type_vtables () in
      TypeArgsHashtbl.add instantiations mir_type_args vtables;
      vtables
    | Some vtables -> vtables
end
