open Basic_collections
open Mir
open Mir_adt
open Mir_type

module BlockBuilder = struct
  type t = {
    id: Block.id;
    source: Block.source;
    (* Instructions in the block currently being built, in reverse *)
    mutable instructions: cf_instruction list;
    mutable phis: (Type.t * cf_var * cf_var IMap.t) list;
    mutable next: cf_var Block.next;
  }
end

(* Break block id and continue block id for a loop *)
type loop_context = Block.id * Block.id

type t = {
  (* Data structures for MIR *)
  mutable main_id: Block.id;
  mutable blocks: BlockBuilder.t IMap.t;
  mutable globals: cf_var Global.t SMap.t;
  mutable funcs: Function.t SMap.t;
  mutable types: Aggregate.t SMap.t;
  mutable modules: Module.t SMap.t;
  mutable current_block_builder: BlockBuilder.t option;
  mutable current_module_builder: Module.t option;
  mutable current_block_source: Block.source;
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
  (* ADT signature id to its corresponding MirADT record *)
  mutable adt_sig_to_mir_adt: MirADT.t IMap.t;
  (* All tuple types used in this program, keyed by their type arguments *)
  mutable tuple_instantiations: Aggregate.t TypeArgsHashtbl.t;
}

let mk () =
  {
    main_id = 0;
    blocks = IMap.empty;
    globals = SMap.empty;
    funcs = SMap.empty;
    types = SMap.empty;
    modules = SMap.empty;
    current_block_builder = None;
    current_module_builder = None;
    current_block_source = GlobalInit "";
    current_block_sequence_ids = [];
    current_loop_contexts = [];
    adt_sig_to_mir_adt = IMap.empty;
    tuple_instantiations = TypeArgsHashtbl.create 10;
  }

let builders_to_blocks builders =
  IMap.map
    (fun builder ->
      {
        Block.id = builder.BlockBuilder.id;
        instructions = List.rev builder.instructions;
        phis = builder.phis;
        next = builder.next;
        source = builder.source;
      })
    builders

let add_global ~ecx global =
  let name = global.Global.name in
  let builder = Option.get ecx.current_module_builder in
  ecx.globals <- SMap.add name global ecx.globals;
  builder.globals <- SSet.add name builder.globals

let add_function ~ecx func =
  let name = func.Function.name in
  let builder = Option.get ecx.current_module_builder in
  ecx.funcs <- SMap.add name func ecx.funcs;
  builder.funcs <- SSet.add name builder.funcs

let emit ~ecx inst =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder -> builder.instructions <- (mk_instr_id (), inst) :: builder.instructions

let emit_phi ~ecx value_type var_id args =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder -> builder.phis <- (value_type, var_id, args) :: builder.phis

let mk_block_builder ~ecx =
  let block_id = mk_block_id () in
  let builder =
    {
      BlockBuilder.id = block_id;
      source = ecx.current_block_source;
      instructions = [];
      phis = [];
      next = Halt;
    }
  in
  ecx.blocks <- IMap.add block_id builder ecx.blocks;
  builder

let set_block_builder ~ecx builder = ecx.current_block_builder <- Some builder

let get_block_builder_id_throws ~ecx =
  match ecx.current_block_builder with
  | Some { id; _ } -> id
  | None -> failwith "No current block builder"

let start_new_block ~ecx =
  let builder = mk_block_builder ~ecx in
  set_block_builder ~ecx builder;
  builder.id

let finish_block ~ecx next =
  let next =
    match ecx.current_block_builder with
    | Some { instructions = (_, Ret _) :: _; _ } -> Block.Halt
    | _ -> next
  in
  match ecx.current_block_builder with
  | None -> ()
  | Some builder ->
    builder.next <- next;
    ecx.current_block_sequence_ids <- builder.id :: ecx.current_block_sequence_ids;
    ecx.current_block_builder <- None

let finish_block_branch ~ecx test continue jump =
  finish_block ~ecx (Branch { test; continue; jump })

let finish_block_continue ~ecx continue = finish_block ~ecx (Continue continue)

let finish_block_halt ~ecx = finish_block ~ecx Halt

let start_block_sequence ~ecx source =
  ecx.current_block_sequence_ids <- [];
  ecx.current_block_source <- source

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids

let push_loop_context ~ecx break_id continue_id =
  ecx.current_loop_contexts <- (break_id, continue_id) :: ecx.current_loop_contexts

let pop_loop_context ~ecx = ecx.current_loop_contexts <- List.tl ecx.current_loop_contexts

let get_loop_context ~ecx = List.hd ecx.current_loop_contexts

let get_module_builder ~ecx = Option.get ecx.current_module_builder

let start_module ~ecx name =
  ecx.current_module_builder <-
    Some { Module.name; funcs = SSet.empty; globals = SSet.empty; types = SSet.empty }

let end_module ~ecx =
  let mod_ = get_module_builder ~ecx in
  ecx.modules <- SMap.add mod_.name mod_ ecx.modules;
  ecx.current_module_builder <- None

(*
 * Instantiation of generic types
 *)

let get_mir_adt ~ecx (adt_sig : Types.adt_sig) = IMap.find adt_sig.id ecx.adt_sig_to_mir_adt

let add_adt_sig ~ecx (adt_sig : Types.adt_sig) (full_name : string) (name_loc : Loc.t) =
  let mir_adt = MirADT.mk adt_sig full_name name_loc in
  ecx.adt_sig_to_mir_adt <- IMap.add adt_sig.id mir_adt ecx.adt_sig_to_mir_adt;
  mir_adt

let instantiation_key_from_type_args mir_adt type_args =
  if mir_adt.MirADT.is_parameterized then
    Types.Tuple type_args
  else
    Types.Int

(* Instantiate a generic ADT with a particular set of type arguments. If there is already an
   instance of the ADT with those type arguments return its aggregate types. Otherwise create new
   aggregate types for this type instance, then save and return them. *)
let rec instantiate_adt ~ecx mir_adt type_args =
  let open MirADT in
  let adt_sig = mir_adt.adt_sig in
  let mir_type_args = List.map (to_mir_type ~ecx) type_args in
  match TypeArgsHashtbl.find_opt mir_adt.instantiations mir_type_args with
  | Some ctor_to_agg -> ctor_to_agg
  | None ->
    let parameterized_name =
      if mir_adt.is_parameterized then
        let type_args_string = TypeArgs.to_string mir_type_args in
        Printf.sprintf "%s<%s>" mir_adt.name type_args_string
      else
        mir_adt.name
    in
    let mk_variant_name variant_name =
      if has_variants mir_adt then
        Printf.sprintf "%s::%s" parameterized_name variant_name
      else
        parameterized_name
    in
    let mk_variant_loc () =
      if has_variants mir_adt then
        failwith "TODO: Instantiate variants (and fix loc to come from variant) "
      else
        mir_adt.loc
    in
    let type_param_bindings = Types.bind_type_params_to_args adt_sig.type_params type_args in
    let ctor_to_agg =
      SMap.mapi
        (fun variant_name variant_sig ->
          match variant_sig with
          | Types.EnumVariantSig -> failwith "TODO: Instantiate enum variant sig"
          | Types.TupleVariantSig element_sigs ->
            let agg_elements =
              List.map
                (fun element_sig ->
                  let element_ty = Types.substitute_type_params type_param_bindings element_sig in
                  let mir_ty = to_mir_type ~ecx element_ty in
                  (None, mir_ty))
                element_sigs
            in
            let agg =
              mk_aggregate (mk_variant_name variant_name) (mk_variant_loc ()) agg_elements
            in
            add_aggregate ~ecx agg;
            agg
          | Types.RecordVariantSig field_sigs ->
            let agg_elements =
              SMap.fold
                (fun field_name field_sig agg_elements ->
                  let element_ty = Types.substitute_type_params type_param_bindings field_sig in
                  let mir_ty = to_mir_type ~ecx element_ty in
                  (Some field_name, mir_ty) :: agg_elements)
                field_sigs
                []
            in
            (* Preserve order of fields in source code *)
            let mir_variant = SMap.find variant_name mir_adt.variants in
            let field_locs = mir_variant.field_locs in
            let sorted_agg_elements =
              List.sort
                (fun (name1, _) (name2, _) ->
                  Loc.compare
                    (SMap.find (Option.get name1) field_locs)
                    (SMap.find (Option.get name2) field_locs))
                agg_elements
            in
            let agg =
              mk_aggregate (mk_variant_name variant_name) (mk_variant_loc ()) sorted_agg_elements
            in
            add_aggregate ~ecx agg;
            agg)
        adt_sig.variant_sigs
    in
    TypeArgsHashtbl.add mir_adt.instantiations mir_type_args ctor_to_agg;
    ctor_to_agg

(* Instantiate a tuple with a particular set of element types. If a tuple with these element types
   has already been instantiated, return its aggregate type. Otherwise create new aggregate type for
   this tuple, save, and return it. *)
and instantiate_tuple ~ecx element_types =
  let mir_type_args = List.map (to_mir_type ~ecx) element_types in
  match TypeArgsHashtbl.find_opt ecx.tuple_instantiations mir_type_args with
  | Some agg -> agg
  | None ->
    let type_args_string = TypeArgs.to_string mir_type_args in
    let name = "$tuple<" ^ type_args_string ^ ">" in
    let agg_elements = List.map (fun mir_ty -> (None, mir_ty)) mir_type_args in
    let agg = mk_aggregate name Loc.none agg_elements in
    TypeArgsHashtbl.add ecx.tuple_instantiations mir_type_args agg;
    add_aggregate ~ecx agg;
    agg

and to_mir_type ~ecx ty =
  match ty with
  | Types.Unit -> `UnitT
  | Types.Bool -> `BoolT
  | Types.Byte -> `ByteT
  | Types.Int -> `IntT
  | Types.Long -> `LongT
  | Types.IntLiteral { resolved; _ } -> to_mir_type ~ecx (Option.get resolved)
  | Types.String -> `StringT
  | Types.Array element_ty -> `PointerT (to_mir_type ~ecx element_ty)
  | Types.Function _ -> `FunctionT
  | Types.Tuple elements ->
    let tuple_agg = instantiate_tuple ~ecx elements in
    `PointerT (`AggregateT tuple_agg)
  | Types.ADT { adt_sig; type_args } ->
    let mir_adt = get_mir_adt ~ecx adt_sig in
    let variant_aggs = instantiate_adt ~ecx mir_adt type_args in
    (* TODO: Handle variant types *)
    let (_, agg) = SMap.choose variant_aggs in
    `PointerT (`AggregateT agg)
  | Types.TVar _
  | Types.TypeParam _
  | Types.Any ->
    failwith "Not allowed as value in IR"

and add_aggregate ~ecx agg =
  let name = agg.Aggregate.name in
  let builder = Option.get ecx.current_module_builder in
  ecx.types <- SMap.add name agg ecx.types;
  builder.types <- SSet.add name builder.types
