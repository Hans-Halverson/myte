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

let mk_mir_tuple_layout ~(ecx : Ecx.t) decl_node =
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let element_sigs = Types.get_tuple_variant adt_sig id.name in
  let layout =
    {
      MirAdtAggregateLayout.template = TupleTemplate element_sigs;
      instantiations = mk_aggregate_layout_instantiations adt_sig;
    }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout = Aggregate layout }

let mk_mir_record_layout ~(ecx : Ecx.t) decl_node record_decl_node =
  let open Ast.TypeDeclaration.Record in
  let (id, full_name, adt_sig) = get_type_node_properties ~ecx decl_node in
  let field_locs =
    List.fold_left
      (fun field_locs { Field.loc; name = { name; _ }; _ } -> SMap.add name loc field_locs)
      SMap.empty
      record_decl_node.fields
  in
  let field_sigs = Types.get_record_variant adt_sig id.name in
  let field_sigs_and_locs =
    SMap.mapi (fun name sig_ty -> (sig_ty, SMap.find name field_locs)) field_sigs
  in
  let layout =
    {
      MirAdtAggregateLayout.template = RecordTemplate field_sigs_and_locs;
      instantiations = mk_aggregate_layout_instantiations adt_sig;
    }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout = Aggregate layout }
