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
      | (_, Tuple [single_element]) -> can_inline_single_element_tuple root_adt_sig single_element
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
          MirAdtAggregateLayout.template = TupleTemplate element_sigs;
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
        MirAdtAggregateLayout.template = RecordTemplate field_sigs_and_locs;
        instantiations = mk_aggregate_layout_instantiations adt_sig;
      }
  in
  { MirAdtLayout.name = full_name; loc = id.loc; adt_sig; layout }
