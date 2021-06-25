open Basic_collections
open Mir
open Mir_adt
open Mir_type

module BlockBuilder = struct
  type t = {
    id: Block.id;
    func: label;
    (* Instructions in the block currently being built, in reverse *)
    mutable instructions: cf_instruction list;
    mutable phis: (Type.t * cf_var * cf_var IMap.t) list;
    mutable next: cf_var Block.next;
  }
end

(* Break block id and continue block id for a loop *)
type loop_context = Block.id * Block.id

type t = {
  pcx: Program_context.t;
  (* Data structures for MIR *)
  mutable main_id: Block.id;
  mutable blocks: BlockBuilder.t IMap.t;
  mutable globals: cf_var Global.t SMap.t;
  mutable funcs: Function.t SMap.t;
  mutable types: Aggregate.t SMap.t;
  mutable modules: Module.t SMap.t;
  mutable current_block_builder: BlockBuilder.t option;
  mutable current_func: label;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
  (* ADT signature id to its corresponding MirADT record *)
  mutable adt_sig_to_mir_adt: MirADT.t IMap.t;
  (* All tuple types used in this program, keyed by their type arguments *)
  mutable tuple_instantiations: Aggregate.t TypeArgsHashtbl.t;
  (* All instances of generic functions used in this program that have been generated so far,
     uniquely identifier by the generic function name and its type arguments. *)
  mutable func_instantiations: unit TypeArgsHashtbl.t SMap.t;
  (* All instances of generic functions that must still be generated. This must be empty for the
     emit pass to be complete. Keys are the generic function name and its type arguments, and
     value is a map of type parameter bindings to be used when generating the function instance. *)
  mutable pending_func_instantiations: Types.t IMap.t TypeArgsHashtbl.t SMap.t;
  (* Concrete types bound to type parameters in the current context. This is used when generating
     generic functions. *)
  mutable current_type_param_bindings: Types.t IMap.t;
  (* All function declaration AST nodes, indexed by their full name *)
  mutable func_decl_nodes: Ast.Function.t SMap.t;
  (* Whether we are currently emitting blocks for the init function *)
  mutable in_init: bool;
  (* The last init block builder that was completed *)
  mutable last_init_block_builder: BlockBuilder.t option;
}

let mk ~pcx =
  {
    pcx;
    main_id = 0;
    blocks = IMap.empty;
    globals = SMap.empty;
    funcs = SMap.empty;
    types = SMap.empty;
    modules = SMap.empty;
    current_block_builder = None;
    current_func = "";
    current_loop_contexts = [];
    adt_sig_to_mir_adt = IMap.empty;
    tuple_instantiations = TypeArgsHashtbl.create 10;
    func_instantiations = SMap.empty;
    pending_func_instantiations = SMap.empty;
    current_type_param_bindings = IMap.empty;
    func_decl_nodes = SMap.empty;
    in_init = false;
    last_init_block_builder = None;
  }

let builders_to_blocks builders =
  IMap.map
    (fun builder ->
      {
        Block.id = builder.BlockBuilder.id;
        instructions = List.rev builder.instructions;
        phis = builder.phis;
        next = builder.next;
        func = builder.func;
      })
    builders

let add_global ~ecx global =
  let name = global.Global.name in
  ecx.globals <- SMap.add name global ecx.globals

let add_function ~ecx func =
  let name = func.Function.name in
  ecx.funcs <- SMap.add name func ecx.funcs

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
      func = ecx.current_func;
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
    ecx.current_block_builder <- None;
    if ecx.in_init then ecx.last_init_block_builder <- Some builder

let finish_block_branch ~ecx test continue jump =
  finish_block ~ecx (Branch { test; continue; jump })

let finish_block_continue ~ecx continue = finish_block ~ecx (Continue continue)

let finish_block_halt ~ecx = finish_block ~ecx Halt

let set_current_func ~ecx func = ecx.current_func <- func

let push_loop_context ~ecx break_id continue_id =
  ecx.current_loop_contexts <- (break_id, continue_id) :: ecx.current_loop_contexts

let pop_loop_context ~ecx = ecx.current_loop_contexts <- List.tl ecx.current_loop_contexts

let get_loop_context ~ecx = List.hd ecx.current_loop_contexts

let emit_init_section ~ecx f =
  let old_in_init = ecx.in_init in
  let old_func = ecx.current_func in
  ecx.in_init <- true;
  ecx.current_func <- init_func_name;

  let init_block_id = start_new_block ~ecx in

  (* If an init section has already been created, link its last block to the new init section *)
  (match ecx.last_init_block_builder with
  | Some last_init_block_builder -> last_init_block_builder.next <- Continue init_block_id
  | None -> ());

  (* Run callback to generate init instructions and finish block *)
  let result = f () in
  finish_block_halt ~ecx;

  ecx.in_init <- old_in_init;
  ecx.current_func <- old_func;
  result

(*
 * Generic Types
 *)

let get_mir_adt ~ecx (adt_sig : Types.adt_sig) = IMap.find adt_sig.id ecx.adt_sig_to_mir_adt

let add_adt_sig ~ecx (adt_sig : Types.adt_sig) (full_name : string) (name_loc : Loc.t) =
  let mir_adt = MirADT.mk adt_sig full_name name_loc in
  ecx.adt_sig_to_mir_adt <- IMap.add adt_sig.id mir_adt ecx.adt_sig_to_mir_adt;
  mir_adt

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
  ecx.types <- SMap.add name agg ecx.types

(*
 * Generic Functions
 *)

(* Find the representative type for a particular type, which may be a type parameter bound to a
   concrete type if we are generating an instantiation of a generic function.
  
   Every rep type that is a type parameter is guaranteed to have a concrete type substituted for it. *)
let find_rep_non_generic_type ~ecx ty =
  let ty = Type_context.find_rep_type ~cx:ecx.pcx.Program_context.type_ctx ty in
  if IMap.is_empty ecx.current_type_param_bindings then
    ty
  else
    Types.substitute_type_params ecx.current_type_param_bindings ty

let add_necessary_func_instantiation ~ecx name type_params arg_tvar_ids =
  let arg_rep_tys =
    List.map (fun arg_tvar_id -> find_rep_non_generic_type ~ecx (TVar arg_tvar_id)) arg_tvar_ids
  in
  let arg_mir_tys = List.map (fun arg_rep_ty -> to_mir_type ~ecx arg_rep_ty) arg_rep_tys in
  let name_with_args =
    let type_args_string = TypeArgs.to_string arg_mir_tys in
    Printf.sprintf "%s<%s>" name type_args_string
  in
  let already_instantiated =
    match SMap.find_opt name ecx.func_instantiations with
    | None -> false
    | Some instantiated_type_args ->
      (match TypeArgsHashtbl.find_opt instantiated_type_args arg_mir_tys with
      | None -> false
      | Some _ -> true)
  in
  ( if already_instantiated then
    ()
  else
    let pending_type_args =
      match SMap.find_opt name ecx.pending_func_instantiations with
      | None -> TypeArgsHashtbl.create 2
      | Some pending_instantiation_type_args -> pending_instantiation_type_args
    in
    match TypeArgsHashtbl.find_opt pending_type_args arg_mir_tys with
    | Some _ -> ()
    | None ->
      let instantiated_type_param_bindings =
        List.fold_left2
          (fun type_param_bindings { Types.TypeParam.id; _ } arg_rep_ty ->
            IMap.add id arg_rep_ty type_param_bindings)
          ecx.current_type_param_bindings
          type_params
          arg_rep_tys
      in
      TypeArgsHashtbl.add pending_type_args arg_mir_tys instantiated_type_param_bindings;
      ecx.pending_func_instantiations <-
        SMap.add name pending_type_args ecx.pending_func_instantiations );
  name_with_args

let pop_pending_func_instantiation ~ecx =
  match SMap.choose_opt ecx.pending_func_instantiations with
  | None -> None
  | Some (name, pending_type_args) ->
    let (type_args, type_param_bindings) =
      let pending_type_args_iter = TypeArgsHashtbl.to_seq pending_type_args in
      match pending_type_args_iter () with
      | Seq.Cons (args_and_bindings, _) -> args_and_bindings
      | _ -> failwith "Pending type args table must be nonempty"
    in
    if TypeArgsHashtbl.length pending_type_args = 1 then
      ecx.pending_func_instantiations <- SMap.remove name ecx.pending_func_instantiations
    else
      TypeArgsHashtbl.remove pending_type_args type_args;
    let name_with_args =
      let type_args_string = TypeArgs.to_string type_args in
      Printf.sprintf "%s<%s>" name type_args_string
    in
    Some (name, name_with_args, type_param_bindings)

let in_type_binding_context ~ecx type_param_bindings f =
  let old_type_param_bindings = ecx.current_type_param_bindings in
  ecx.current_type_param_bindings <-
    IMap.union (fun _ p1 _ -> Some p1) type_param_bindings ecx.current_type_param_bindings;
  f ();
  ecx.current_type_param_bindings <- old_type_param_bindings

let add_function_declaration_node ~ecx name decl_node =
  ecx.func_decl_nodes <- SMap.add name decl_node ecx.func_decl_nodes
