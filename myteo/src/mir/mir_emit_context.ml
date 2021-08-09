open Basic_collections
open Mir
open Mir_adt_layout
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
  mutable current_block_builder: BlockBuilder.t option;
  mutable current_func: label;
  mutable current_in_std_lib: bool;
  mutable current_is_main: bool;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
  (* ADT signature id to its corresponding MIR layout *)
  mutable adt_sig_to_mir_layout: MirAdtLayout.t IMap.t;
  (* All tuple types used in this program, keyed by their type arguments *)
  mutable tuple_instantiations: Aggregate.t TypeArgsHashtbl.t;
  (* All instances of generic functions used in this program that have been generated so far,
     uniquely identifier by the generic function name and its type arguments. *)
  mutable func_instantiations: unit TypeArgsHashtbl.t SMap.t;
  (* All instances of generic functions that must still be generated. This must be empty for the
     emit pass to be complete. Keys are the generic function name and its type arguments, and
     value is a map of type parameter bindings to be used when generating the function instance. *)
  mutable pending_func_instantiations: Types.Type.t IMap.t TypeArgsHashtbl.t SMap.t;
  (* Concrete types bound to type parameters in the current context. This is used when generating
     generic functions. *)
  mutable current_type_param_bindings: Types.Type.t IMap.t;
  (* All function declaration AST nodes, indexed by their full name *)
  mutable func_decl_nodes: Ast.Function.t SMap.t;
  (* Whether we are currently emitting blocks for the init function *)
  mutable in_init: bool;
  (* The last init block builder that was completed *)
  mutable last_init_block_builder: BlockBuilder.t option;
  mutable max_string_literal_id: int;
}

let mk ~pcx =
  {
    pcx;
    main_id = 0;
    blocks = IMap.empty;
    globals = SMap.empty;
    funcs = SMap.empty;
    types = SMap.empty;
    current_block_builder = None;
    current_func = "";
    current_in_std_lib = false;
    current_is_main = false;
    current_loop_contexts = [];
    adt_sig_to_mir_layout = IMap.empty;
    tuple_instantiations = TypeArgsHashtbl.create 10;
    func_instantiations = SMap.empty;
    pending_func_instantiations = SMap.empty;
    current_type_param_bindings = IMap.empty;
    func_decl_nodes = SMap.empty;
    in_init = false;
    last_init_block_builder = None;
    max_string_literal_id = 0;
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

let add_string_literal ~ecx loc string =
  let name =
    let id = ecx.max_string_literal_id in
    ecx.max_string_literal_id <- id + 1;
    let prefix =
      if ecx.current_in_std_lib then
        Mir.std_lib_string_prefix
      else
        ".S"
    in
    prefix ^ string_of_int id
  in
  let length = String.length string in
  let ty = `ArrayT (`ByteT, length) in
  let global = { Global.loc; name; ty; init_val = Some (`ArrayL (`ByteT, length, string)) } in
  add_global ~ecx global;
  `PointerL (ty, name)

(*
 * Generic Types
 *)

let get_mir_adt_layout ~ecx (adt_sig : Types.AdtSig.t) =
  IMap.find adt_sig.id ecx.adt_sig_to_mir_layout

let add_mir_adt_layout ~ecx (mir_adt_layout : MirAdtLayout.t) =
  ecx.adt_sig_to_mir_layout <-
    IMap.add mir_adt_layout.adt_sig.id mir_adt_layout ecx.adt_sig_to_mir_layout

(* Instantiate a MIR aggregate layout with a particular set of type arguments. If there is already
   an instance of the ADT with those type arguments return its aggregate type. Otherwise create new
   aggregate type for this type instance, then save and return it. *)
let rec instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args =
  let open MirAdtLayout in
  let adt_sig = mir_adt_layout.adt_sig in
  let { MirAdtAggregateLayout.template; instantiations } =
    match mir_adt_layout.layout with
    | Aggregate aggregate_layout -> aggregate_layout
    | _ -> failwith "Expected aggregate layout"
  in
  let instantiate_aggregate_elements type_param_bindings =
    match template with
    | MirAdtAggregateLayout.TupleTemplate element_sigs ->
      List.map
        (fun element_sig ->
          let element_ty = Types.substitute_type_params type_param_bindings element_sig in
          let mir_ty = to_mir_type ~ecx element_ty in
          (None, mir_ty))
        element_sigs
    | RecordTemplate field_sigs_and_locs ->
      let aggregate_elements_and_locs =
        SMap.fold
          (fun field_name (field_sig, loc) agg_elements ->
            let element_ty = Types.substitute_type_params type_param_bindings field_sig in
            let mir_ty = to_mir_type ~ecx element_ty in
            ((Some field_name, mir_ty), loc) :: agg_elements)
          field_sigs_and_locs
          []
      in
      (* Preserve order of fields in source code *)
      List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) aggregate_elements_and_locs
      |> List.map fst
  in
  match instantiations with
  (* Concrete layout has already been instantiated - return already created aggregate *)
  | Concrete { contents = Some aggregate } -> aggregate
  (* Concrete layout has not yet been instantiated - create and return aggregate *)
  | Concrete instantiated_ref ->
    let aggregate_elements = instantiate_aggregate_elements IMap.empty in
    let aggregate = mk_aggregate ~ecx mir_adt_layout.name mir_adt_layout.loc aggregate_elements in
    instantiated_ref := Some aggregate;
    aggregate
  (* Check if generic layout has already been instantiated with these type args, create if not *)
  | Generic instantiations ->
    let mir_type_args = List.map (to_mir_type ~ecx) type_args in
    (match TypeArgsHashtbl.find_opt instantiations mir_type_args with
    | Some aggregate -> aggregate
    | None ->
      let parameterized_name = mir_adt_layout.name ^ TypeArgs.to_string mir_type_args in
      let type_param_bindings = Types.bind_type_params_to_args adt_sig.type_params type_args in
      let aggregate_elements = instantiate_aggregate_elements type_param_bindings in
      let aggregate = mk_aggregate ~ecx parameterized_name mir_adt_layout.loc aggregate_elements in
      TypeArgsHashtbl.add instantiations mir_type_args aggregate;
      aggregate)

and instantiate_mir_adt_inline_value_layout ~ecx mir_adt_layout type_args =
  let open MirAdtLayout in
  let adt_sig = mir_adt_layout.adt_sig in
  match mir_adt_layout.layout with
  | InlineValue inline_ty ->
    let type_param_bindings = Types.bind_type_params_to_args adt_sig.type_params type_args in
    let ty = Types.substitute_type_params type_param_bindings inline_ty in
    to_mir_type ~ecx ty
  | _ -> failwith "Expected aggregate layout"

(* Instantiate a tuple with a particular set of element types. If a tuple with these element types
   has already been instantiated, return its aggregate type. Otherwise create new aggregate type for
   this tuple, save, and return it. *)
and instantiate_tuple ~ecx element_types =
  let mir_type_args = List.map (to_mir_type ~ecx) element_types in
  match TypeArgsHashtbl.find_opt ecx.tuple_instantiations mir_type_args with
  | Some agg -> agg
  | None ->
    let type_args_string = TypeArgs.to_string mir_type_args in
    let name = "$tuple" ^ type_args_string in
    let agg_elements = List.map (fun mir_ty -> (None, mir_ty)) mir_type_args in
    let agg = mk_aggregate ~ecx name Loc.none agg_elements in
    TypeArgsHashtbl.add ecx.tuple_instantiations mir_type_args agg;
    agg

and to_mir_type ~ecx ty =
  match ty with
  | Types.Type.Unit -> `UnitT
  | Bool -> `BoolT
  | Byte -> `ByteT
  | Int -> `IntT
  | Long -> `LongT
  | IntLiteral { resolved; _ }
  | TraitBound { resolved; _ } ->
    to_mir_type ~ecx (Option.get resolved)
  | Array element_ty -> `PointerT (to_mir_type ~ecx element_ty)
  | Function _ -> `FunctionT
  | Tuple elements ->
    let tuple_agg = instantiate_tuple ~ecx elements in
    `PointerT (`AggregateT tuple_agg)
  | ADT { adt_sig; type_args } ->
    let mir_adt_layout = get_mir_adt_layout ~ecx adt_sig in
    (match mir_adt_layout.layout with
    | MirAdtLayout.Aggregate _ ->
      let aggregate = instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args in
      `PointerT (`AggregateT aggregate)
    | InlineValue _ -> instantiate_mir_adt_inline_value_layout ~ecx mir_adt_layout type_args)
  | TypeParam { name = Explicit name; _ } -> failwith ("Not allowed as value in IR " ^ name)
  | TypeParam _
  | TVar _
  | Any ->
    failwith "Not allowed as value in IR"

and mk_aggregate ~ecx name loc elements =
  let aggregate = { Aggregate.id = mk_aggregate_id (); name; loc; elements } in
  ecx.types <- SMap.add name aggregate ecx.types;
  aggregate

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

let add_necessary_func_instantiation ~ecx name key_type_params key_type_args =
  let key_type_args = List.map (find_rep_non_generic_type ~ecx) key_type_args in
  let arg_mir_tys = List.map (fun key_arg_ty -> to_mir_type ~ecx key_arg_ty) key_type_args in
  let name_with_args =
    let type_args_string = TypeArgs.to_string arg_mir_tys in
    Printf.sprintf "%s%s" name type_args_string
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
          key_type_params
          key_type_args
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
      Printf.sprintf "%s%s" name type_args_string
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
