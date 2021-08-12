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

let mk_layout_instantiations (adt_sig : Types.AdtSig.t) =
  if adt_sig.type_params <> [] then
    Generic (TypeArgsHashtbl.create 4)
  else
    Concrete (ref None)

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

let get_record_field_locs record_node =
  let open Ast.TypeDeclaration.Record in
  List.fold_left
    (fun field_locs { Field.loc; name = { name; _ }; _ } -> SMap.add name loc field_locs)
    SMap.empty
    record_node.fields

let mk_mir_tuple_layout ~(ecx : Ecx.t) decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let element_sigs = Types.get_tuple_variant adt_sig id.name in
  let layout =
    match element_sigs with
    | [single_sig] when Opts.optimize () && can_inline_single_element_tuple adt_sig single_sig ->
      MirAdtLayout.InlineValue single_sig
    | _ ->
      Aggregate
        { template = TupleTemplate element_sigs; instantiations = mk_layout_instantiations adt_sig }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }

let mk_mir_record_layout ~(ecx : Ecx.t) decl_node record_decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let field_sigs = Types.get_record_variant adt_sig id.name in
  let field_locs = get_record_field_locs record_decl_node in
  let field_sigs_and_locs =
    SMap.mapi (fun name sig_ty -> (sig_ty, SMap.find name field_locs)) field_sigs
  in
  let layout =
    MirAdtLayout.Aggregate
      {
        template = RecordTemplate field_sigs_and_locs;
        instantiations = mk_layout_instantiations adt_sig;
      }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }

let mk_mir_variants_layout ~(ecx : Ecx.t) decl_node variant_nodes =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  (* Collect variants and sort by order they are defined in source *)
  let (variants, has_data_variant) =
    SMap.fold
      (fun _ { Types.AdtSig.Variant.name; loc; kind } (variants, has_data_variant) ->
        match kind with
        | Enum -> ((name, loc) :: variants, has_data_variant)
        | Tuple _
        | Record _ ->
          ((name, loc) :: variants, true))
      adt_sig.variants
      ([], false)
  in
  let variants = List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) variants in
  (* Assign integer tags to each variant, of the smallest possible integer type *)
  let num_variants = List.length variants in
  let tag_mir_type =
    if Integers.is_out_of_unsigned_byte_range (Int64.of_int num_variants) then
      `IntT
    else
      `ByteT
  in
  let (_, tags, variant_locs) =
    List.fold_left
      (fun (i, tags, variant_locs) (name, loc) ->
        let tag =
          match tag_mir_type with
          | `ByteT ->
            (* Convert signed byte to equivalent unsigned byte *)
            if i >= 128 then
              `ByteL (127 - i)
            else
              `ByteL i
          | `IntT -> `IntL (Int32.of_int i)
        in
        (i + 1, SMap.add name tag tags, SMap.add name loc variant_locs))
      (0, SMap.empty, SMap.empty)
      variants
  in
  (* If there were no data variants, this is a pure enum variant represented as a single integer *)
  if not has_data_variant then
    let layout = MirAdtLayout.PureEnum { tag_mir_type; tags } in
    { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }
  else
    (* Find field locs for all record variants *)
    let field_locs =
      List.fold_left
        (fun acc variant_node ->
          match variant_node with
          | Ast.TypeDeclaration.RecordVariant record_node ->
            let field_locs = get_record_field_locs record_node in
            SMap.add record_node.name.name field_locs acc
          | _ -> acc)
        SMap.empty
        variant_nodes
    in
    (* Build templates for all data variants *)
    let templates =
      SMap.fold
        (fun name variant_sig acc ->
          match variant_sig.Types.AdtSig.Variant.kind with
          | Enum -> acc
          | Tuple element_sigs -> SMap.add name (TupleTemplate element_sigs) acc
          | Record field_sigs ->
            let field_locs = SMap.find name field_locs in
            let field_sigs_and_locs =
              SMap.mapi (fun name sig_ty -> (sig_ty, SMap.find name field_locs)) field_sigs
            in
            SMap.add name (RecordTemplate field_sigs_and_locs) acc)
        adt_sig.variants
        SMap.empty
    in
    let layout =
      MirAdtLayout.Variants
        {
          tag_mir_type;
          tags;
          templates;
          instantiations = mk_layout_instantiations adt_sig;
          variant_locs;
        }
    in
    { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }
