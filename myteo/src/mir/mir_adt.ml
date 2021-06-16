open Basic_collections
open Mir

module MirVariant = struct
  type t = {
    name: string;
    loc: Loc.t;
    (* If this is a record variant, store record field locs *)
    field_locs: Loc.t SMap.t;
  }

  let mk name loc field_locs = { name; loc; field_locs }
end

module MirADT = struct
  type t = {
    adt_sig: Types.adt_sig;
    name: string;
    loc: Loc.t;
    (* All variants for this ADT *)
    mutable variants: MirVariant.t SMap.t;
    (* All instantations of this ADT that have created (aka configurations of type arguments that
       have been used). For each, store a map from each constructor name to the aggregate type
       corresponding to the constructor instantiated with those type arguments. *)
    mutable instantiations: Aggregate.t SMap.t Types.TypeHashtbl.t;
    (* Whether this type is parameterized in MIR (meaning it contains type parameters that affect
       the aggregate type). *)
    is_parameterized: bool;
  }

  let rec mk adt_sig name loc =
    let estimated_size =
      if adt_sig.Types.type_params = [] then
        1
      else
        4
    in
    {
      adt_sig;
      name;
      loc;
      variants = SMap.empty;
      instantiations = Types.TypeHashtbl.create estimated_size;
      is_parameterized = is_mir_parameterized adt_sig;
    }

  (* Whether or not this ADT needs to be parameterized in MIR. A type only needs to be parameterized
     if it stores a type parameter in one of its variants, as this means the aggregate type may vary
     between instances. *)
  and is_mir_parameterized adt_sig =
    let open Types in
    let has_variable_size ty =
      match ty with
      | TypeParam _ -> true
      | Any
      | Unit
      | Bool
      | Byte
      | Int
      | Long
      | String
      | TVar _
      | IntLiteral _
      (* Stored as pointers, so size does not vary *)
      | Array _
      | Function _
      (* Tuples and ADTs are stored behind a pointer, so size does not vary *)
      | Tuple _
      | ADT _ ->
        false
    in
    SMap.exists
      (fun _ variant_sig ->
        match variant_sig with
        | EnumVariantSig -> false
        | TupleVariantSig element_sigs -> List.exists has_variable_size element_sigs
        | RecordVariantSig field_sigs ->
          SMap.exists (fun _ field_sig -> has_variable_size field_sig) field_sigs)
      adt_sig.variant_sigs

  let has_variants mir_adt = SMap.cardinal mir_adt.adt_sig.variant_sigs > 1

  let add_adt_variant mir_adt name loc field_locs =
    let mir_variant = MirVariant.mk name loc field_locs in
    mir_adt.variants <- SMap.add name mir_variant mir_adt.variants
end
