open Basic_collections
open Mir_adt_layout
open Mir_type
module Ecx = Mir_emit_context

let get_type_node_properties ~(ecx : Ecx.t) decl_node =
  let { Ast.TypeDeclaration.name = { Ast.Identifier.loc; _ } as id; _ } = decl_node in
  let binding = Bindings.get_type_binding ecx.pcx.bindings loc in
  let full_name = String.concat "." (binding.module_ @ [binding.name]) in
  let type_decl = Bindings.get_type_decl binding in
  (id, full_name, type_decl.adt_sig)

let mk_aggregate_layout_instantiations (adt_sig : Types.AdtSig.t) =
  if adt_sig.type_params <> [] then
    MirAdtAggregateLayout.Generic (TypeArgsHashtbl.create 4)
  else
    MirAdtAggregateLayout.Concrete (ref None)

(* A single element tuple can be inlined as long as it does not contain its own ADT type as the
   single element. Must recurse through other inlined single element tuples as it may be cyclic. *)
let rec can_inline_single_element_tuple (root_adt_sig : Types.AdtSig.t) (element_ty : Types.Type.t)
    =
  match element_ty with
  | Types.Type.ADT { adt_sig; _ } when adt_sig.id = root_adt_sig.id -> false
  | ADT { adt_sig; _ } ->
    (* Recurse into other inlined non-variant single element tuples *)
    if SMap.cardinal adt_sig.variants = 1 then
      match SMap.choose adt_sig.variants with
      | (_, { kind = Tuple [single_element]; _ }) ->
        can_inline_single_element_tuple root_adt_sig single_element
      | _ -> true
    else
      true
  | _ -> true

let mk_mir_tuple_layout ~(ecx : Ecx.t) decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let element_sigs = Types.get_tuple_variant adt_sig id.name in
  let layout =
    match element_sigs with
    | [single_sig] when Opts.optimize () && can_inline_single_element_tuple adt_sig single_sig ->
      MirAdtLayout.InlineValue single_sig
    | _ ->
      Aggregate
        {
          template = TupleTemplate element_sigs;
          instantiations = mk_aggregate_layout_instantiations adt_sig;
        }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }

let mk_mir_record_layout ~(ecx : Ecx.t) decl_node record_decl_node =
  let open Ast.TypeDeclaration.Record in
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let field_sigs = Types.get_record_variant adt_sig id.name in
  let field_locs =
    List.fold_left
      (fun field_locs { Field.loc; name = { name; _ }; _ } -> SMap.add name loc field_locs)
      SMap.empty
      record_decl_node.fields
  in
  let field_sigs_and_locs =
    SMap.mapi (fun name sig_ty -> (sig_ty, SMap.find name field_locs)) field_sigs
  in
  let layout =
    MirAdtLayout.Aggregate
      {
        template = RecordTemplate field_sigs_and_locs;
        instantiations = mk_aggregate_layout_instantiations adt_sig;
      }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }

let mk_mir_variants_layout ~(ecx : Ecx.t) decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  (* Split enum and data variants, sorting by order they are defined in source *)
  let (enum_variants, data_variants) =
    SMap.fold
      (fun _ { Types.AdtSig.Variant.name; loc; kind } (enum_variants, data_variants) ->
        match kind with
        | Enum -> ((name, loc) :: enum_variants, data_variants)
        | Tuple _
        | Record _ ->
          (enum_variants, (name, loc) :: data_variants))
      adt_sig.variants
      ([], [])
  in
  let sort_variants = List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) in
  let enum_variants = sort_variants enum_variants in
  let data_variants = sort_variants data_variants in
  (* If there are only enum variants a pure enum layout is possible, where each variant is
     represented by a single integer of the smallest possible type. *)
  if data_variants = [] then
    let num_variants = List.length enum_variants in
    let mir_type =
      if Integers.is_out_of_unsigned_byte_range (Int64.of_int num_variants) then
        `IntT
      else
        `ByteT
    in
    let (_, tags) =
      List.fold_left
        (fun (i, tags) (name, _) ->
          let tag =
            match mir_type with
            | `ByteT ->
              (* Convert signed byte to equivalent unsigned byte *)
              if i >= 128 then
                `ByteL (127 - i)
              else
                `ByteL i
            | `IntT -> `IntL (Int32.of_int i)
          in
          (i + 1, SMap.add name tag tags))
        (0, SMap.empty)
        enum_variants
    in
    let layout = MirAdtLayout.PureEnum { mir_type; tags } in
    { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }
  else
    failwith "TODO: Emit MIR layout for variants with data"
