open Basic_collections
open Mir_adt_layout
open Mir_builders
open Mir_emit_utils
open Mir_type
open Mir_type_args_hashtbl
module Ecx = Mir_emit_context

let get_type_node_properties ~(ecx : Ecx.t) decl_node =
  let { Ast.TypeDeclaration.name = { Ast.Identifier.loc; _ } as id; _ } = decl_node in
  let binding = Bindings.get_type_binding ecx.pcx.bindings loc in
  let full_name = mk_type_binding_name binding in
  let type_decl = Bindings.get_type_decl binding in
  (id, full_name, type_decl.adt_sig)

let mk_layouts (adt_sig : Types.AdtSig.t) =
  if adt_sig.type_params <> [] then
    MirAdtLayout.Generic (TypeArgsHashtbl.create 4)
  else
    Concrete (ref None)

let get_record_field_locs record_node =
  let open Ast.TypeDeclaration.Record in
  List.fold_left
    (fun field_locs { Field.loc; name = { name; _ }; _ } -> SMap.add name loc field_locs)
    SMap.empty
    record_node.fields

let mk_mir_tuple_layout ~(ecx : Ecx.t) decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let element_sigs = Types.get_tuple_variant adt_sig id.name in
  let template = MirAdtLayout.SingleTemplate (TupleTemplate element_sigs) in
  let layouts = mk_layouts adt_sig in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; template; layouts }

let mk_mir_record_layout ~(ecx : Ecx.t) decl_node record_decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let field_sigs = Types.get_record_variant adt_sig id.name in
  let field_locs = get_record_field_locs record_decl_node in
  let field_sigs_and_locs =
    SMap.mapi
      (fun name field_sig -> (field_sig.Types.AdtSig.Variant.type_, SMap.find name field_locs))
      field_sigs
  in
  let template = MirAdtLayout.SingleTemplate (RecordTemplate field_sigs_and_locs) in
  let layouts = mk_layouts adt_sig in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; template; layouts }

let mk_mir_variants_layout ~(ecx : Ecx.t) decl_node variant_nodes =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  (* Collect variants and sort by order they are defined in source *)
  let variants =
    SMap.fold
      (fun _ { Types.AdtSig.Variant.name; loc; _ } variants -> (name, loc) :: variants)
      adt_sig.variants
      []
  in
  let variants = List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) variants in
  (* Assign integer tags to each variant, of the smallest possible integer type *)
  let num_variants = List.length variants in
  let tag_mir_type =
    if Integers.is_out_of_unsigned_byte_range (Int64.of_int num_variants) then
      Type.Int
    else
      Byte
  in
  let (_, tags, variant_locs) =
    List.fold_left
      (fun (i, tags, variant_locs) (name, loc) ->
        let tag =
          match tag_mir_type with
          | Type.Byte ->
            (* Convert signed byte to equivalent unsigned byte *)
            let i =
              if i >= 128 then
                127 - i
              else
                i
            in
            mk_byte_lit (Int8.of_int i)
          | Int -> mk_int_lit i
          | _ -> failwith "Invalid tag MIR type"
        in
        (i + 1, SMap.add name tag tags, SMap.add name loc variant_locs))
      (0, SMap.empty, SMap.empty)
      variants
  in
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
            SMap.mapi
              (fun name field_sig ->
                (field_sig.Types.AdtSig.Variant.type_, SMap.find name field_locs))
              field_sigs
          in
          SMap.add name (RecordTemplate field_sigs_and_locs) acc)
      adt_sig.variants
      SMap.empty
  in
  let template = MirAdtLayout.VariantsTemplate { tag_mir_type; tags; templates; variant_locs } in
  let layouts = mk_layouts adt_sig in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; template; layouts }
