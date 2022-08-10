open Basic_collections
open Mir
open Mir_builders
open Mir_adt_layout
open Mir_emit_utils
open Mir_trait_object_layout
open Mir_type
open Mir_type_args_hashtbl
module BVMap = Bindings.BVMap
module BVSet = Bindings.BVSet

(* Break block and continue block for a loop *)
type loop_context = Block.t * Block.t

(* Immutable strings are deduplicated in MIR, and have a canonical value global and length global *)
module ImmutableString = struct
  type t = {
    value_global_val: (* Pointer *) Value.t;
    size_global_val: (* Pointer *) Value.t;
  }
end

(* Information about the function that is currently being emitted *)
module FunctionContext = struct
  type t = {
    return_ty: Types.Type.t;
    return_block: Block.t;
    return_pointer: Value.t option;
    captures: Bindings.LBVMMap.VSet.t;
    env_agg: Aggregate.t option;
    env_ptr: Value.t option;
    mutable num_anonymous_functions: int;
  }

  let null =
    {
      return_ty = Unit;
      return_block = null_block;
      return_pointer = None;
      captures = Bindings.LBVMMap.VSet.empty;
      env_agg = None;
      env_ptr = None;
      num_anonymous_functions = 0;
    }
end

module PendingAnonymousFunction = struct
  type t = {
    node: Ast.Expression.AnonymousFunction.t;
    (* Type param bindings to evaluate this function in *)
    type_param_bindings: Types.Type.t IMap.t;
    (* Function argument bindings to their MIR value *)
    arguments: Value.t BVMap.t;
    (* Bindings captured by this function *)
    captures: Bindings.LBVMMap.VSet.t;
    (* Environment type, or None if no bindings were captured *)
    env_agg: Aggregate.t option;
  }
end

type t = {
  pcx: Program_context.t;
  mutable program: Program.t;
  mutable main_label: label;
  mutable current_block: Block.t option;
  mutable current_func: Function.t;
  mutable current_func_context: FunctionContext.t;
  mutable current_in_std_lib: bool;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
  (* Whether to filter out the standard library when dumping IR or asm *)
  filter_std_lib: bool;
  (* Local variable binding to the StackAlloc or myte_alloc instruction that defines that variable *)
  mutable local_variable_to_alloc_instr: Value.t BVMap.t;
  (* Function parameter binding to the argument value for that parameter *)
  mutable param_to_argument: Value.t BVMap.t;
  (* ADT signature id to its corresponding MIR layout *)
  mutable adt_sig_to_mir_layout: MirAdtLayout.t IMap.t;
  (* Trait signature id to its corresponding trait object vtables *)
  mutable trait_sig_to_trait_object_layout: MirTraitObjectLayout.t IMap.t;
  (* Names for all nongeneric functions that are pending generation *)
  mutable pending_nongeneric_funcs: FunctionSet.t;
  (* Map from pending anonymous functions to their AST node, the type param binding context, and
     the params to argument context. *)
  mutable pending_anonymous_funcs: PendingAnonymousFunction.t FunctionMap.t;
  (* Names for all trampoline functions that are pending generation *)
  mutable pending_trampoline_funcs: FunctionSet.t;
  (* All AST nodes for all globals that should be generated, indexed by their full name *)
  mutable pending_globals: Ast.Statement.VariableDeclaration.t SMap.t;
  (* All tuple types used in this program, keyed by their type arguments *)
  mutable tuple_instantiations: Aggregate.t TypeArgsHashtbl.t;
  (* All instances of generic functions used in this program that have been generated so far,
     uniquely identifier by the generic function name and its type arguments. *)
  mutable generic_func_instantiations: Function.t TypeArgsHashtbl.t SMap.t;
  (* All instances of generic functions that must still be generated. This must be empty for the
     emit pass to be complete. Keys are the generic function name and its type arguments, and value
     is the function and a map of type parameter bindings to be used when generating the function
     instance. *)
  mutable pending_generic_func_instantiations:
    (Function.t * Types.Type.t IMap.t) TypeArgsHashtbl.t SMap.t;
  (* Concrete types bound to type parameters in the current context. This is used when generating
     generic functions. *)
  mutable current_type_param_bindings: Types.Type.t IMap.t;
  (* All function declaration AST nodes, indexed by their full name *)
  mutable func_decl_nodes: Ast.Function.t SMap.t;
  (* All globals in program indexed by their binding *)
  mutable global_variable_decl_nodes: Ast.Statement.VariableDeclaration.t BVMap.t;
  (* Whether we are currently emitting blocks for the init function *)
  mutable in_init: bool;
  (* The last init block that was completed *)
  mutable last_init_block: Block.t option;
  mutable max_mutable_string_literal_id: int;
  mutable max_mutable_std_string_literal_id: int;
  mutable max_immutable_string_literal_id: int;
  mutable max_immutable_std_string_literal_id: int;
  (* Map from string to the deduplicated immutable string *)
  mutable immutable_string_literals: ImmutableString.t SMap.t;
}

let mk_aggregate ~ecx name loc elements =
  let aggregate = { Aggregate.id = mk_aggregate_id (); name; loc; elements } in
  ecx.program.types <- SMap.add name aggregate ecx.program.types;
  aggregate

(* Create a placeholder aggregate (during type instantiation) that will be filled in later *)
let mk_placeholder_aggregate ~ecx name loc = mk_aggregate ~ecx name loc []

let mk ~pcx =
  let program =
    {
      Program.globals = SMap.empty;
      funcs = SMap.empty;
      types = SMap.empty;
      main_func = null_function;
    }
  in
  {
    pcx;
    program;
    main_label = "";
    current_block = None;
    current_func = null_function;
    current_func_context = FunctionContext.null;
    current_in_std_lib = false;
    current_loop_contexts = [];
    filter_std_lib =
      (Opts.dump_ir ()
      || Opts.dump_untransformed_ir ()
      || Opts.dump_virtual_asm ()
      || Opts.dump_asm ()
      || Opts.dump_full_asm ())
      && not (Opts.dump_stdlib ());
    local_variable_to_alloc_instr = BVMap.empty;
    param_to_argument = BVMap.empty;
    adt_sig_to_mir_layout = IMap.empty;
    trait_sig_to_trait_object_layout = IMap.empty;
    pending_nongeneric_funcs = FunctionSet.empty;
    pending_anonymous_funcs = FunctionMap.empty;
    pending_trampoline_funcs = FunctionSet.empty;
    pending_globals = SMap.empty;
    tuple_instantiations = TypeArgsHashtbl.create 10;
    generic_func_instantiations = SMap.empty;
    pending_generic_func_instantiations = SMap.empty;
    current_type_param_bindings = IMap.empty;
    func_decl_nodes = SMap.empty;
    global_variable_decl_nodes = BVMap.empty;
    in_init = false;
    last_init_block = None;
    max_mutable_string_literal_id = 0;
    max_mutable_std_string_literal_id = 0;
    max_immutable_string_literal_id = 0;
    max_immutable_std_string_literal_id = 0;
    immutable_string_literals = SMap.empty;
  }

let trampoline_prefix = "_trampoline$"

let mk_trampoline_name name = trampoline_prefix ^ name

let strip_trampoline_name name =
  let name_length = String.length name in
  let prefix_length = String.length trampoline_prefix in
  String.sub name prefix_length (name_length - prefix_length)

let add_global ~ecx global =
  let name = global.Global.name in
  ecx.program.globals <- SMap.add name global ecx.program.globals

let mk_block ~ecx = mk_block ~func:ecx.current_func

let get_current_block ~ecx : Block.t =
  match ecx.current_block with
  | None -> failwith "No current block"
  | Some block -> block

let set_current_block ~ecx block = ecx.current_block <- Some block

let start_new_block ~ecx =
  let block = mk_block ~ecx in
  set_current_block ~ecx block;
  block

let finish_block ~ecx f =
  match ecx.current_block with
  | None -> ()
  | Some current_block ->
    (match get_terminator current_block with
    | Some _ -> ()
    | None ->
      f current_block;
      ecx.current_block <- None;
      if ecx.in_init then ecx.last_init_block <- Some current_block)

let finish_block_ret ~ecx ~(arg : Value.t option) =
  finish_block ~ecx (fun current_block -> mk_ret_ ~block:current_block ~arg)

let finish_block_branch ~ecx (test : Value.t) (continue : Block.t) (jump : Block.t) =
  finish_block ~ecx (fun current_block ->
      continue.prev_blocks <- BlockSet.add current_block continue.prev_blocks;
      jump.prev_blocks <- BlockSet.add current_block jump.prev_blocks;
      mk_branch_ ~block:current_block ~test ~continue ~jump)

let finish_block_continue ~ecx (continue : Block.t) =
  finish_block ~ecx (fun current_block ->
      continue.prev_blocks <- BlockSet.add current_block continue.prev_blocks;
      mk_continue_ ~block:current_block ~continue)

let finish_block_unreachable ~ecx =
  finish_block ~ecx (fun current_block -> mk_unreachable_ ~block:current_block)

let push_loop_context ~ecx break_block continue_block =
  ecx.current_loop_contexts <- (break_block, continue_block) :: ecx.current_loop_contexts

let pop_loop_context ~ecx = ecx.current_loop_contexts <- List.tl ecx.current_loop_contexts

let get_loop_context ~ecx = List.hd ecx.current_loop_contexts

let is_captured_binding ~ecx binding =
  Bindings.LBVMMap.VSet.mem binding ecx.current_func_context.captures

let get_local_ptr_def_instr ~ecx use_loc type_ =
  let binding = Bindings.get_value_binding ecx.pcx.bindings use_loc in
  match BVMap.find_opt binding ecx.local_variable_to_alloc_instr with
  | Some instr -> instr
  | None ->
    (* Captured bindings must be stored on heap, all other bindings are stored on stack *)
    let instr =
      if binding.is_captured && Bindings.is_mutable_variable binding then
        mk_blockless_call_builtin Mir_builtin.myte_alloc [mk_int_lit_of_int32 Int32.one] [type_]
      else
        mk_blockless_stack_alloc ~type_
    in
    ecx.local_variable_to_alloc_instr <- BVMap.add binding instr ecx.local_variable_to_alloc_instr;
    instr

let add_function_argument ~ecx ~func binding type_ =
  let argument = mk_argument ~func ~decl_loc:binding.Bindings.ValueBinding.loc ~type_ in
  ecx.param_to_argument <- BVMap.add binding argument ecx.param_to_argument;
  argument

let get_function_argument_value ~ecx binding = BVMap.find binding ecx.param_to_argument

let emit_init_section ~ecx f =
  let old_in_init = ecx.in_init in
  let old_func = ecx.current_func in
  ecx.in_init <- true;
  ecx.current_func <- SMap.find init_func_name ecx.program.funcs;

  let init_block = start_new_block ~ecx in

  (* If an init section has already been created, link its last block to the new init section *)
  (match ecx.last_init_block with
  | Some last_init_block ->
    (match get_terminator last_init_block with
    | Some term_instr -> term_instr.instr <- Continue init_block
    | None -> mk_continue_ ~block:last_init_block ~continue:init_block);
    init_block.prev_blocks <- BlockSet.add last_init_block init_block.prev_blocks
  | None -> ());

  (* Run callback to generate init instructions and finish block *)
  let result = f () in
  finish_block_ret ~ecx ~arg:None;

  ecx.in_init <- old_in_init;
  ecx.current_func <- old_func;
  result

(*
 * String literals
 *)

let add_mutable_string_literal ~ecx loc string : Value.t =
  let name =
    if ecx.current_in_std_lib then (
      let id = ecx.max_mutable_std_string_literal_id in
      ecx.max_mutable_std_string_literal_id <- id + 1;
      Mir.std_lib_string_prefix ^ string_of_int id
    ) else
      let id = ecx.max_mutable_string_literal_id in
      ecx.max_mutable_string_literal_id <- id + 1;
      ".S" ^ string_of_int id
  in
  let length = String.length string in
  let global =
    mk_global
      ~loc
      ~name
      ~type_:(Array (Byte, length))
      ~init_val:(Some (mk_array_string_lit string))
      ~is_constant:true
  in
  add_global ~ecx global;
  global.value

let add_immutable_string_literal ~ecx string =
  match SMap.find_opt string ecx.immutable_string_literals with
  (* Immutable strings have a single canonical global value and size *)
  | Some imm_string -> imm_string
  | None ->
    (* Create global for string value *)
    let value_name =
      if ecx.current_in_std_lib then (
        let id = ecx.max_immutable_std_string_literal_id in
        ecx.max_immutable_std_string_literal_id <- id + 1;
        Mir.std_lib_immutable_string_prefix ^ string_of_int id
      ) else
        let id = ecx.max_immutable_string_literal_id in
        ecx.max_immutable_string_literal_id <- id + 1;
        ".IS" ^ string_of_int id
    in
    let size = String.length string in
    let value_ty = Type.Array (Byte, size) in
    let value_global =
      mk_global
        ~loc:Loc.none
        ~name:value_name
        ~type_:value_ty
        ~init_val:(Some (mk_array_string_lit string))
        ~is_constant:true
    in
    add_global ~ecx value_global;

    (* Create global for string size *)
    let size_name = value_name ^ "Size" in
    let size_global =
      mk_global
        ~loc:Loc.none
        ~name:size_name
        ~type_:Int
        ~init_val:(Some (mk_int_lit size))
        ~is_constant:true
    in
    add_global ~ecx size_global;

    (* Intern globals for new immutable string *)
    let imm_string =
      { ImmutableString.value_global_val = value_global.value; size_global_val = size_global.value }
    in
    ecx.immutable_string_literals <- SMap.add string imm_string ecx.immutable_string_literals;
    imm_string

(*
 * Functions
 *)

let mk_empty_function ~(ecx : t) ~(name : string) : Function.t =
  let func = mk_function ~name in
  ecx.program.funcs <- SMap.add name func ecx.program.funcs;
  func

let start_function ~(ecx : t) ~(func : Function.t) ~loc ~params ~return_type =
  ecx.current_func <- func;
  (* Set properties of current function *)
  func.loc <- loc;
  func.params <- params;
  func.return_type <- return_type;
  (* Create start block for function *)
  let start_block = start_new_block ~ecx in
  func.start_block <- start_block

let start_function_context ~ecx ~return_ty ~return_block ~return_pointer ~env_agg ~env_ptr ~captures
    =
  ecx.current_func_context <-
    {
      return_ty;
      return_block;
      return_pointer;
      env_agg;
      env_ptr;
      captures;
      num_anonymous_functions = 0;
    }

(*
 * Generic Types
 *)
let add_mir_adt_layout ~ecx (mir_adt_layout : MirAdtLayout.t) =
  ecx.adt_sig_to_mir_layout <-
    IMap.add mir_adt_layout.adt_sig.id mir_adt_layout ecx.adt_sig_to_mir_layout

let rec get_mir_adt_layout ~ecx (adt_sig : Types.AdtSig.t) (type_args : Types.Type.t list) =
  let mir_adt_layout = IMap.find adt_sig.id ecx.adt_sig_to_mir_layout in
  match mir_adt_layout.layouts with
  | Concrete { contents = Some layout } -> layout
  | Concrete ({ contents = None } as layout_ref) ->
    instantiate_mir_adt_layout ~ecx mir_adt_layout type_args (fun layout ->
        layout_ref := Some layout)
  | Generic layouts ->
    (match TypeArgsHashtbl.find_opt layouts type_args with
    | Some layout -> layout
    | None ->
      instantiate_mir_adt_layout ~ecx mir_adt_layout type_args (fun layout ->
          TypeArgsHashtbl.add layouts type_args layout))

(* Instantiate a MIR layout with a particular set of type arguments. If there is already an instance
   of the ADT with those type arguments return its instance type. Otherwise create new instance type
   type for this type instance, then save and return it.

   In cases where we need to instantiate the type with a new set of type arguments, create the
   resulting instance type as a placeholder and insert it into tables before checking element types,
   as type may be recursive.

   The same general logic for instantiation applies to aggregate layouts (where the instance is
   a single aggregate), and variants layouts (where the instance is an aggregate for the overall
   enum, and variants for all data variants).*)
and instantiate_mir_adt_layout
    ~ecx
    (mir_adt_layout : MirAdtLayout.t)
    (type_args : Types.Type.t list)
    (set_layout : MirAdtLayout.layout -> unit) =
  let type_param_bindings =
    Types.bind_type_params_to_args mir_adt_layout.adt_sig.type_params type_args
  in
  match mir_adt_layout.template with
  | SingleTemplate (TupleTemplate [element])
    when Opts.optimize () && can_inline_single_element_tuple mir_adt_layout.adt_sig element ->
    let ty = Types.substitute_type_params type_param_bindings element in
    (match to_mir_type ~ecx ty with
    | None -> MirAdtLayout.ZeroSize
    | Some mir_type -> InlineValue mir_type)
  | SingleTemplate ((TupleTemplate _ | RecordTemplate _) as template) ->
    let parameterized_name = mir_adt_layout.name ^ TypeArgs.to_string ~pcx:ecx.pcx type_args in
    let aggregate = mk_placeholder_aggregate ~ecx parameterized_name mir_adt_layout.loc in
    set_layout (Aggregate aggregate);
    let elements = instantiate_mir_adt_template_elements ~ecx template type_param_bindings in
    if elements <> [] then (
      let (elements, _) = align_and_pad_aggregate_elements elements in
      aggregate.elements <- elements;
      Aggregate aggregate
    ) else (
      (* If aggregate has only zero size elements then entire layout is zero size. Remove
         placeholder aggregate that was temporarily created. This is safe, meaning no other types
         can reference the placeholder aggregate, because if they did this would not have had only
         zero size elements. *)
      ecx.program.types <- SMap.remove parameterized_name ecx.program.types;
      set_layout ZeroSize;
      ZeroSize
    )
  | VariantsTemplate { tags; tag_mir_type; templates; _ } when SMap.is_empty templates ->
    PureEnum { tags; tag_mir_type }
  | VariantsTemplate { tags; tag_mir_type; templates; variant_locs } ->
    let instantiate_mir_adt_variants_layout () =
      let parameterized_adt_name =
        mir_adt_layout.name ^ TypeArgs.to_string ~pcx:ecx.pcx type_args
      in
      let tag_element = mk_tag_element tag_mir_type in

      let union_aggregate =
        mk_placeholder_aggregate ~ecx parameterized_adt_name mir_adt_layout.loc
      in
      let layout =
        {
          VariantsLayout.tags;
          tag_mir_type;
          union = union_aggregate;
          size = 0;
          variants = SMap.empty;
        }
      in
      set_layout (Variants layout);

      let (variant_elements, max_non_ptr_elements_size, max_num_ptr_elements) =
        SMap.fold
          (fun variant_name
               template
               (variant_elements, max_non_ptr_elements_size, max_num_ptr_elements) ->
            let elements =
              instantiate_mir_adt_template_elements ~ecx template type_param_bindings
            in

            (* Split elements into pointer and non-pointer elements, preserving order *)
            let (ptr_elements, non_ptr_elements) =
              List.partition (fun (_, mir_type) -> is_pointer_type mir_type) elements
            in

            (* Add tag element to start of non-pointer elements, then align and insert padding *)
            let non_ptr_elements = tag_element :: non_ptr_elements in
            let ((_, non_ptr_elements_size) as non_ptr_elements) =
              align_and_pad_aggregate_elements non_ptr_elements
            in

            let max_non_ptr_elements_size = max max_non_ptr_elements_size non_ptr_elements_size in
            let max_num_ptr_elements = max max_num_ptr_elements (List.length ptr_elements) in

            let variant_elements =
              (variant_name, non_ptr_elements, ptr_elements) :: variant_elements
            in
            (variant_elements, max_non_ptr_elements_size, max_num_ptr_elements))
          templates
          ([], 0, 0)
      in

      (* Non-ptr element section size is rounded up to pointer size if there are any pointer elements *)
      let max_non_ptr_elements_size =
        if max_num_ptr_elements = 0 then
          max_non_ptr_elements_size
        else
          round_up_to_alignment max_non_ptr_elements_size ptr_size
      in
      let max_ptr_elements_size = ptr_size * max_num_ptr_elements in
      let max_size = max_non_ptr_elements_size + max_ptr_elements_size in

      let variant_aggregates =
        List.fold_left
          (fun acc (variant_name, (non_ptr_elements, non_ptr_elements_size), ptr_elements) ->
            (* Add end padding to non-ptr element section to match largest non-ptr element section *)
            let non_ptr_elements =
              add_end_padding non_ptr_elements non_ptr_elements_size max_non_ptr_elements_size
            in

            (* Add pointer padding elements to ptr element section to match largest ptr element section *)
            let num_ptr_padding_elements = max_num_ptr_elements - List.length ptr_elements in
            let ptr_padding_elements = List_utils.make num_ptr_padding_elements pointer_element in
            let ptr_elements = ptr_elements @ ptr_padding_elements in

            (* Create aggregates for each data variant *)
            let aggregate_name = parameterized_adt_name ^ "::" ^ variant_name in
            let loc = SMap.find variant_name variant_locs in
            let elements = non_ptr_elements @ ptr_elements in
            let aggregate = mk_aggregate ~ecx aggregate_name loc elements in
            SMap.add variant_name aggregate acc)
          SMap.empty
          variant_elements
      in

      layout.variants <- variant_aggregates;
      layout.size <- max_size;

      (* Create aggregates for union (and enum variants) by padding tag element to match largest
         non-ptr element section, then adding pointer elements for all ptrs in ptr element section. *)
      let tag_size = size_of_type tag_mir_type in
      let union_non_ptr_elements =
        add_end_padding [tag_element] tag_size max_non_ptr_elements_size
      in
      let union_ptr_elements = List_utils.make max_num_ptr_elements pointer_element in
      union_aggregate.elements <- union_non_ptr_elements @ union_ptr_elements;

      MirAdtLayout.Variants layout
    in

    let is_option = String.equal mir_adt_layout.name Std_lib.std_option_option in
    if is_option then
      let element_ty = Types.substitute_type_params type_param_bindings (List.hd type_args) in
      (* Niche is already used if element is option with mir representation of inlined pointer *)
      let is_element_option =
        match element_ty with
        | ADT { adt_sig; _ } ->
          let mir_adt_layout = IMap.find adt_sig.id ecx.adt_sig_to_mir_layout in
          String.equal mir_adt_layout.name Std_lib.std_option_option
        | _ -> false
      in
      (* Use inlined niche for some compatible options *)
      match to_mir_type ~ecx element_ty with
      | Some (Pointer element_type) when not is_element_option ->
        let none_lit = mk_null_ptr_lit element_type in
        let niches = SMap.singleton "None" none_lit in
        let type_ = Type.Pointer element_type in
        InlineValueWithNiche
          { type_; inlined_type = type_; niches; inline_range = NotEqual none_lit }
      | Some Bool ->
        let none_lit = mk_byte_lit 2 in
        let niches = SMap.singleton "None" none_lit in
        InlineValueWithNiche
          { type_ = Byte; inlined_type = Bool; niches; inline_range = Below none_lit }
      | Some Byte ->
        let none_lit = mk_int_lit_of_int32 256l in
        let niches = SMap.singleton "None" none_lit in
        InlineValueWithNiche
          { type_ = Int; inlined_type = Byte; niches; inline_range = Below none_lit }
      | Some Int ->
        let none_lit = mk_long_lit 0x100000000L in
        let niches = SMap.singleton "None" none_lit in
        InlineValueWithNiche
          { type_ = Long; inlined_type = Int; niches; inline_range = Below none_lit }
      | _ -> instantiate_mir_adt_variants_layout ()
    else
      instantiate_mir_adt_variants_layout ()

(* Instantiate a template with a set of type parameters, generating packed elements for aggregate *)
and instantiate_mir_adt_template_elements ~ecx template type_param_bindings =
  let elements =
    match template with
    | TupleTemplate element_sigs ->
      List_utils.filter_mapi
        (fun i element_sig ->
          let element_ty = Types.substitute_type_params type_param_bindings element_sig in
          match to_mir_type ~ecx element_ty with
          | None -> None
          | Some mir_type -> Some (TupleKeyCache.get_key i, mir_type))
        element_sigs
    | RecordTemplate field_sigs_and_locs ->
      let aggregate_elements_and_locs =
        SMap.fold
          (fun field_name (field_sig, loc) agg_elements ->
            let element_ty = Types.substitute_type_params type_param_bindings field_sig in
            match to_mir_type ~ecx element_ty with
            | None -> agg_elements
            | Some mir_type -> ((field_name, mir_type), loc) :: agg_elements)
          field_sigs_and_locs
          []
      in
      (* Preserve order of fields in source code *)
      List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) aggregate_elements_and_locs
      |> List.map fst
  in

  (* Pack elements by grouping elements in increasing order of alignment *)
  order_elements_by_alignment elements

(* Instantiate a tuple with a particular set of element types. If a tuple with these element types
   has already been instantiated, return its aggregate type. Otherwise create new aggregate type for
   this tuple, save, and return it.

   If resulting tuple has no fields return None instead of creating an aggregate. *)
and instantiate_tuple ~ecx element_types =
  match TypeArgsHashtbl.find_opt ecx.tuple_instantiations element_types with
  | Some agg -> Some agg
  | None ->
    let agg_elements =
      List_utils.filter_mapi
        (fun i element_type ->
          match to_mir_type ~ecx element_type with
          | None -> None
          | Some mir_type -> Some (TupleKeyCache.get_key i, mir_type))
        element_types
    in
    if agg_elements = [] then
      None
    else
      let type_args_string = TypeArgs.to_string ~pcx:ecx.pcx element_types in
      let name = "$tuple" ^ type_args_string in
      let agg = mk_aggregate ~ecx name Loc.none agg_elements in
      TypeArgsHashtbl.add ecx.tuple_instantiations element_types agg;
      Some agg

and get_closure_global_value ~ecx ~loc ~func =
  let closure_global_name = "_closure$" ^ func.Function.name in
  match SMap.find_opt closure_global_name ecx.program.globals with
  | Some global -> global.value
  | None ->
    let closure_ty = get_closure_type ~ecx in
    let global =
      mk_global
        ~loc
        ~name:closure_global_name
        ~type_:(get_closure_type ~ecx)
        ~init_val:(Some (mk_aggregate_closure closure_ty func))
        ~is_constant:false
    in
    add_global ~ecx global;
    global.value

and get_trait_object_layout ~ecx (trait_sig : Types.TraitSig.t) =
  match IMap.find_opt trait_sig.id ecx.trait_sig_to_trait_object_layout with
  | Some trait_object -> trait_object
  (* Create trait object if it does not yet exist *)
  | None ->
    let (vtable_size, vtable_indices) = build_vtable_indices trait_sig in

    (* Create aggregate for trait object *)
    let trait_binding = Type_context.get_type_binding ~cx:ecx.pcx.type_ctx trait_sig.loc in
    let full_trait_name = mk_type_binding_name trait_binding in
    let trait_object_label = "_object$" ^ full_trait_name in
    let trait_object_agg =
      mk_aggregate
        ~ecx
        trait_object_label
        trait_sig.loc
        [("item", Pointer Byte); ("vtable", Pointer (Array (Function, vtable_size)))]
    in

    let trait_object_layout =
      MirTraitObjectLayout.mk ~trait_sig ~trait_object_agg ~vtable_indices
    in
    ecx.trait_sig_to_trait_object_layout <-
      IMap.add trait_sig.id trait_object_layout ecx.trait_sig_to_trait_object_layout;
    trait_object_layout

and instantiate_trait_object_vtable ~ecx trait_instance ty =
  let { Types.TraitSig.trait_sig; type_args } = trait_instance in
  let trait_type_args =
    List.map (fun type_arg -> find_rep_non_generic_type ~ecx type_arg) type_args
  in

  (* Get all vtables for this trait instance, creating new trait object layout if it does not yet exist *)
  let trait_object_layout = get_trait_object_layout ~ecx trait_sig in
  let trait_instance =
    match trait_object_layout.instantiations with
    | Concrete trait_instance -> trait_instance
    | Generic instantiations ->
      MirTraitObjectLayout.add_instantiation instantiations trait_type_args
  in

  (* Return vtable for this type if it has already been instantiated, otherwise instantiate it *)
  let type_key = [ty] in
  match TypeArgsHashtbl.find_opt trait_instance type_key with
  | Some trait_object_instance -> trait_object_instance
  | None ->
    (* Determine type of item in trait object. Default to pointer field (for alignment) if type
       is zero width and has no MIR representation. *)
    let (item_type, is_boxed) =
      match to_mir_type ~ecx ty with
      (* Pointers do not need to be boxed and can be used directly *)
      | Some ((Pointer _ | Function) as item_type) -> (item_type, false)
      (* All other types need to be boxed so pointer can be in trait object *)
      | Some item_type -> (Pointer item_type, true)
      | None -> (Pointer (get_zero_size_type ~ecx), false)
    in

    (* Instantiate type's methods that are part of the trait object, building vtable array *)
    let vtable_functions =
      SMap.fold
        (fun method_name method_sig vtable_functions ->
          if Types.MethodSig.is_generic method_sig then
            vtable_functions
          else
            let method_name =
              get_method_function_value
                ~ecx
                ~method_name
                ~super_method_sig:None
                ~receiver_ty:ty
                ~method_instance_type_args:[]
              |> cast_to_function_literal
            in
            (* If item is boxed, add trampoline to load the item before calling the method *)
            let method_name =
              if is_boxed then
                get_trampoline_function ~ecx method_name
              else
                method_name
            in
            method_name :: vtable_functions)
        trait_sig.methods
        []
    in
    let vtable_val = mk_array_vtable_lit (List.rev vtable_functions) in
    let vtable_mir_type = type_of_value vtable_val in

    (* Create vtable and trait object names, composed from fully parameterized type and trait names *)
    let (adt_sig, adt_type_args) =
      match ty with
      | ADT { adt_sig; type_args; _ } -> (adt_sig, type_args)
      | _ -> (Std_lib.get_primitive_adt_sig ty, [])
    in

    (* Find fully parameterized type name *)
    let adt_binding = Type_context.get_type_binding ~cx:ecx.pcx.type_ctx adt_sig.loc in
    let full_adt_name =
      mk_type_binding_name adt_binding ^ TypeArgs.to_string ~pcx:ecx.pcx adt_type_args
    in

    (* Find fully parameterized trait name *)
    let trait_binding = Type_context.get_type_binding ~cx:ecx.pcx.type_ctx trait_sig.loc in
    let full_trait_name =
      mk_type_binding_name trait_binding ^ TypeArgs.to_string ~pcx:ecx.pcx trait_type_args
    in

    (* Create global for vtable and save pointer to it *)
    let vtable_label = Printf.sprintf "_vtable$%s$%s" full_adt_name full_trait_name in
    let vtable_global =
      mk_global
        ~loc:adt_sig.loc
        ~name:vtable_label
        ~type_:vtable_mir_type
        ~init_val:(Some vtable_val)
        ~is_constant:true
    in
    add_global ~ecx vtable_global;
    let vtable = vtable_global.value in

    (* Create aggregate type for type's trait object *)
    let agg_label = Printf.sprintf "_object$%s$%s" full_adt_name full_trait_name in
    let (agg_elements, _) =
      align_and_pad_aggregate_elements [("item", item_type); ("vtable", Pointer vtable_mir_type)]
    in
    let agg = mk_aggregate ~ecx agg_label adt_sig.loc agg_elements in

    let trait_object_instance = { MirTraitObjectLayout.vtable; agg; is_boxed } in
    TypeArgsHashtbl.add trait_instance type_key trait_object_instance;
    trait_object_instance

(* 
 * Convert a type to a MIR type if possible.
 * 
 * If type has zero size in MIR, then return None instead of a MIR type.
 *)
and to_mir_type ~ecx (ty : Types.Type.t) : Type.t option =
  match ty with
  | Types.Type.Unit
  | Never ->
    None
  | Bool -> Some Bool
  | Byte -> Some Byte
  | Int -> Some Int
  | Long -> Some Long
  | Double -> Some Double
  | IntLiteral { resolved; _ }
  | BoundedExistential { resolved; _ } ->
    to_mir_type ~ecx (Option.get resolved)
  | Function _ -> Some (Pointer (get_closure_type ~ecx))
  | Tuple elements ->
    instantiate_tuple ~ecx elements
    |> Option.map (fun tuple_agg -> Type.Pointer (Aggregate tuple_agg))
  | ADT { adt_sig; type_args = [element_ty] } when adt_sig == !Std_lib.array_adt_sig ->
    let mir_type =
      match to_mir_type ~ecx element_ty with
      | Some mir_type -> mir_type
      | None -> get_zero_size_type ~ecx
    in
    Some (Pointer mir_type)
  | ADT { adt_sig; type_args } ->
    let layout = get_mir_adt_layout ~ecx adt_sig type_args in
    (match layout with
    | Aggregate aggregate -> Some (Pointer (Aggregate aggregate))
    | Variants { union; _ } -> Some (Pointer (Aggregate union))
    | PureEnum { tag_mir_type; _ } -> Some (tag_mir_type :> Type.t)
    | InlineValue mir_type -> Some mir_type
    | InlineValueWithNiche { type_; _ } -> Some type_
    | ZeroSize -> None)
  | TraitObject { trait_sig; _ } ->
    let trait_object_layout = get_trait_object_layout ~ecx trait_sig in
    Some (Pointer (Aggregate trait_object_layout.trait_object_agg))
  | TypeParam _
  | TraitBound _
  | TVar _
  | Any _ ->
    failwith "Not allowed as value in IR"

and get_closure_type ~ecx : Type.t =
  match SMap.find_opt closure_type_name ecx.program.types with
  | Some agg -> Aggregate agg
  | None ->
    let agg =
      mk_aggregate ~ecx closure_type_name Loc.none [("func", Function); ("env", Pointer Byte)]
    in
    Aggregate agg

and get_zero_size_type ~ecx =
  (* Add zero size type if it does not already exist *)
  match SMap.find_opt zero_size_name ecx.program.types with
  | None ->
    let agg = mk_placeholder_aggregate ~ecx zero_size_name Loc.none in
    Aggregate agg
  | Some agg -> Aggregate agg

and get_zero_size_global_pointer ~ecx =
  (* Add zero size global if it does not already exist *)
  let zero_size_type = get_zero_size_type ~ecx in
  let zero_size_global =
    match SMap.find_opt zero_size_name ecx.program.globals with
    | Some global -> global
    | None ->
      let global =
        mk_global
          ~loc:Loc.none
          ~name:zero_size_name
          ~type_:zero_size_type
          ~init_val:None
          ~is_constant:false
      in
      add_global ~ecx global;
      global
  in
  zero_size_global.value

and builtin_functions =
  lazy
    ([
       Std_lib.std_bool_bool_equals;
       Std_lib.std_byte_byte_equals;
       Std_lib.std_byte_byte_toInt;
       Std_lib.std_byte_byte_toLong;
       Std_lib.std_byte_byte_toDouble;
       Std_lib.std_double_double_equals;
       Std_lib.std_double_double_toByte;
       Std_lib.std_double_double_toInt;
       Std_lib.std_double_double_toLong;
       Std_lib.std_gc_collect;
       Std_lib.std_gc_getHeapSize;
       Std_lib.std_int_int_equals;
       Std_lib.std_int_int_toByte;
       Std_lib.std_int_int_toLong;
       Std_lib.std_int_int_toDouble;
       Std_lib.std_long_long_equals;
       Std_lib.std_long_long_toByte;
       Std_lib.std_long_long_toInt;
       Std_lib.std_long_long_toDouble;
       Std_lib.std_memory_array_copy;
       Std_lib.std_memory_array_isNull;
       Std_lib.std_memory_array_new;
       Std_lib.std_io_file_builtin_close;
       Std_lib.std_io_file_builtin_open;
       Std_lib.std_io_file_builtin_read;
       Std_lib.std_io_file_builtin_unlink;
       Std_lib.std_io_file_builtin_write;
       Std_lib.std_sys_exit;
     ]
    |> SSet.of_list)

(*
 * Nongeneric Functions
 *)
and get_nongeneric_function_value ~(ecx : t) (name : label) : Value.t =
  let builtin_functions = Lazy.force_val builtin_functions in
  if SSet.mem name builtin_functions then
    mk_myte_builtin_lit name
  else
    (* Mark function as pending if it has not yet been generated *)
    let func =
      match SMap.find_opt name ecx.program.funcs with
      | Some func -> func
      | None ->
        let func = mk_empty_function ~ecx ~name in
        ecx.pending_nongeneric_funcs <- FunctionSet.add func ecx.pending_nongeneric_funcs;
        func
    in
    func.value

and pop_pending_nongeneric_function ~ecx =
  match FunctionSet.choose_opt ecx.pending_nongeneric_funcs with
  | None -> None
  | Some func ->
    let func_decl = SMap.find func.name ecx.func_decl_nodes in
    Some (func, func_decl)

and mark_pending_nongeneric_function_completed ~ecx func =
  ecx.pending_nongeneric_funcs <- FunctionSet.remove func ecx.pending_nongeneric_funcs

(*
 * Anonymous Functions
 *)
and get_anonymous_function_value ~ecx name anon_func_node env_agg captures : Value.t =
  let func = mk_empty_function ~ecx ~name in
  let pending_anon_func =
    {
      PendingAnonymousFunction.node = anon_func_node;
      type_param_bindings = ecx.current_type_param_bindings;
      arguments = ecx.param_to_argument;
      captures;
      env_agg;
    }
  in
  ecx.pending_anonymous_funcs <- FunctionMap.add func pending_anon_func ecx.pending_anonymous_funcs;
  func.value

and pop_pending_anonymous_function ~ecx =
  match FunctionMap.choose_opt ecx.pending_anonymous_funcs with
  | None -> None
  | Some (anon_func, anon_func_node) -> Some (anon_func, anon_func_node)

and mark_pending_anonymous_function_completed ~ecx func =
  ecx.pending_anonymous_funcs <- FunctionMap.remove func ecx.pending_anonymous_funcs

(*
 * Trampoline Functions
 *)
and get_trampoline_function ~(ecx : t) (inner_func : Function.t) : Function.t =
  (* Mark function as pending if it has not yet been generated *)
  let name = mk_trampoline_name inner_func.name in
  let func =
    match SMap.find_opt name ecx.program.funcs with
    | Some func -> func
    | None ->
      let func = mk_empty_function ~ecx ~name in
      ecx.pending_trampoline_funcs <- FunctionSet.add func ecx.pending_trampoline_funcs;
      func
  in
  func

and pop_pending_trampoline_function ~ecx = FunctionSet.choose_opt ecx.pending_trampoline_funcs

and mark_pending_trampoline_function_completed ~ecx func =
  ecx.pending_trampoline_funcs <- FunctionSet.remove func ecx.pending_trampoline_funcs

(*
 * Generic Functions
 *)

(* Find the representative type for a particular type, which may be a type parameter bound to a
   concrete type if we are generating an instantiation of a generic function.

   Every rep type that is a type parameter is guaranteed to have a concrete type substituted for it. *)
and find_rep_non_generic_type ~ecx ty =
  let ty = Type_context.find_rep_type ~cx:ecx.pcx.Program_context.type_ctx ty in
  if IMap.is_empty ecx.current_type_param_bindings then
    ty
  else
    Types.substitute_type_params ecx.current_type_param_bindings ty

and get_generic_function_value
    ~(ecx : t)
    (name : label)
    (key_type_params : Types.TypeParam.t list)
    (key_type_args : Types.Type.t list) : Value.t =
  let key_type_args = List.map (find_rep_non_generic_type ~ecx) key_type_args in

  let already_instantiated_func_opt =
    match SMap.find_opt name ecx.generic_func_instantiations with
    | None -> None
    | Some instantiated_type_args -> TypeArgsHashtbl.find_opt instantiated_type_args key_type_args
  in
  let func =
    match already_instantiated_func_opt with
    | Some func -> func
    | None ->
      let pending_type_args =
        match SMap.find_opt name ecx.pending_generic_func_instantiations with
        | None -> TypeArgsHashtbl.create 2
        | Some pending_instantiation_type_args -> pending_instantiation_type_args
      in
      (match TypeArgsHashtbl.find_opt pending_type_args key_type_args with
      | Some (func, _) -> func
      | None ->
        let name_with_args =
          let type_args_string = TypeArgs.to_string ~pcx:ecx.pcx key_type_args in
          Printf.sprintf "%s%s" name type_args_string
        in
        let func = mk_empty_function ~ecx ~name:name_with_args in
        let instantiated_type_param_bindings =
          List.fold_left2
            (fun type_param_bindings { Types.TypeParam.id; _ } arg_rep_ty ->
              IMap.add id arg_rep_ty type_param_bindings)
            ecx.current_type_param_bindings
            key_type_params
            key_type_args
        in
        TypeArgsHashtbl.add pending_type_args key_type_args (func, instantiated_type_param_bindings);
        ecx.pending_generic_func_instantiations <-
          SMap.add name pending_type_args ecx.pending_generic_func_instantiations;
        func)
  in
  func.value

and get_method_function_value
    ~ecx
    ~(method_name : string)
    ~(super_method_sig : Types.MethodSig.t option)
    ~(receiver_ty : Types.Type.t)
    ~(method_instance_type_args : Types.Type.t list) =
  let (receiver_adt_sig, receiver_type_args) =
    match receiver_ty with
    | ADT { adt_sig; type_args; _ } -> (adt_sig, type_args)
    | _ -> (Std_lib.get_primitive_adt_sig receiver_ty, [])
  in

  (* Find method sig on the ADT's type trait itself, ignoring super. This may be an inherited method. *)
  let type_trait_method_sig =
    Types.AdtSig.lookup_method receiver_adt_sig method_name |> Option.get
  in
  let type_trait_sig = type_trait_method_sig.trait_sig in

  (* If we are looking for a super method make sure to use it instead *)
  let method_sig =
    match super_method_sig with
    | Some super_method_sig -> super_method_sig
    | None -> type_trait_method_sig
  in
  (* Find the source trait for this method, the trait where the method is defined. *)
  let source_trait_sig = method_sig.source_trait_instance.trait_sig in

  (* Calculate generic keys from trait/type *)
  let (extra_key_type_params, extra_key_type_args) =
    if type_trait_sig == source_trait_sig && Option.is_none super_method_sig then
      (* Method was directly declared on the ADT's trait. Substitute type args for trait's type args *)
      (source_trait_sig.type_params, receiver_type_args)
    else
      (* Find the implemention of the trait where this method is defined, which will contain
         the type args for that trait in terms of the type parameters of the ADT's type trait. *)
      let implemented_trait = ref None in
      List.iter
        (fun type_trait ->
          List.iter
            (fun (_, ({ Types.TraitSig.trait_sig; _ } as instance)) ->
              if trait_sig == source_trait_sig then implemented_trait := Some instance)
            type_trait.Types.TraitSig.implemented)
        receiver_adt_sig.traits;
      let implemented_trait_type_args = (Option.get !implemented_trait).type_args in

      (* Map from ADT's type trait type parameters to the receiver's type args in these implemented
         type args, so that they are in terms of the receiver's own type args. *)
      let bindings = Types.bind_type_params_to_args type_trait_sig.type_params receiver_type_args in
      let source_trait_type_args =
        List.map (Types.substitute_type_params bindings) implemented_trait_type_args
      in

      (* The key args are these mapped source trait instance's type args along with the a `this`
         type of the ADT instance. *)
      let all_key_type_params = source_trait_sig.this_type_param :: source_trait_sig.type_params in
      let all_key_type_args = receiver_ty :: source_trait_type_args in
      (all_key_type_params, all_key_type_args)
  in

  (* Full keys are trait/type args plus method's own generic args *)
  let key_type_params = extra_key_type_params @ method_sig.type_params in
  let key_type_args = extra_key_type_args @ method_instance_type_args in

  (* Find name of method fully qualified by module and trait *)
  let method_binding = Bindings.get_value_binding ecx.pcx.bindings method_sig.loc in
  let fully_qualified_method_name = mk_value_binding_name method_binding in

  if key_type_params = [] then
    get_nongeneric_function_value ~ecx fully_qualified_method_name
  else
    get_generic_function_value ~ecx fully_qualified_method_name key_type_params key_type_args

let pop_pending_generic_func_instantiation ~ecx =
  match SMap.choose_opt ecx.pending_generic_func_instantiations with
  | None -> None
  | Some (name, pending_type_args) ->
    (* Build type paramater substitution *)
    let (type_args, type_param_bindings) =
      let pending_type_args_iter = TypeArgsHashtbl.to_seq pending_type_args in
      match pending_type_args_iter () with
      | Seq.Cons (args_and_bindings, _) -> args_and_bindings
      | _ -> failwith "Pending type args table must be nonempty"
    in

    let (func, _) = TypeArgsHashtbl.find pending_type_args type_args in

    (* Remove from pending instantiations table *)
    if TypeArgsHashtbl.length pending_type_args = 1 then
      ecx.pending_generic_func_instantiations <-
        SMap.remove name ecx.pending_generic_func_instantiations
    else
      TypeArgsHashtbl.remove pending_type_args type_args;

    (* Add to completed instantiations table *)
    let instantiations =
      match SMap.find_opt name ecx.generic_func_instantiations with
      | Some instantiations -> instantiations
      | None ->
        let completed_type_args = TypeArgsHashtbl.create 2 in
        ecx.generic_func_instantiations <-
          SMap.add name completed_type_args ecx.generic_func_instantiations;
        completed_type_args
    in
    TypeArgsHashtbl.add instantiations type_args func;

    Some (name, type_param_bindings)

(*
 * Global Variables
 *)

(*
 * Return the pointer literal value for a given global variable if the variable exists in MIR.
 *
 * If global variable has zero size then instead return None.
 *)
let get_global_pointer ~ecx (binding : Bindings.ValueBinding.t) : Value.t option =
  let name = mk_value_binding_name binding in
  match SMap.find_opt name ecx.program.globals with
  (* Global will be in MIR globals map if it has already been created or is pending *)
  | Some global -> Some global.value
  | None ->
    let var_decl = Bindings.get_var_decl binding in
    (match to_mir_type ~ecx (find_rep_non_generic_type ~ecx (TVar var_decl.tvar)) with
    (* Global does not exist if it has zero size type *)
    | None -> None
    | Some type_ ->
      (* Add global to pending globals queue *)
      let decl_node = BVMap.find binding ecx.global_variable_decl_nodes in
      ecx.pending_globals <- SMap.add name decl_node ecx.pending_globals;

      (* Add global to MIR globals map *)
      let global =
        mk_global
          ~loc:binding.loc
          ~name
          ~type_
          ~init_val:None
          ~is_constant:(decl_node.kind = Immutable)
      in
      add_global ~ecx global;

      Some global.value)

let pop_pending_global ~ecx =
  match SMap.choose_opt ecx.pending_globals with
  | None -> None
  | Some (name, decl) ->
    ecx.pending_globals <- SMap.remove name ecx.pending_globals;
    Some (name, decl)

let in_type_binding_context ~ecx type_param_bindings f =
  let old_type_param_bindings = ecx.current_type_param_bindings in
  ecx.current_type_param_bindings <-
    IMap.union (fun _ p1 _ -> Some p1) type_param_bindings ecx.current_type_param_bindings;
  f ();
  ecx.current_type_param_bindings <- old_type_param_bindings

let add_function_declaration_node ~ecx name decl_node =
  ecx.func_decl_nodes <- SMap.add name decl_node ecx.func_decl_nodes

let add_global_variable_declaration_node ~ecx decl_loc decl =
  let binding = Bindings.get_value_binding ecx.pcx.bindings decl_loc in
  ecx.global_variable_decl_nodes <- BVMap.add binding decl ecx.global_variable_decl_nodes
