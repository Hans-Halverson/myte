open Basic_collections
open Mir
open Mir_adt_layout
open Mir_emit_utils
open Mir_trait_object_layout
open Mir_type
open Mir_type_args_hashtbl

module BlockBuilder = struct
  type t = {
    id: Block.id;
    func: label;
    (* Instructions in the block currently being built, in reverse *)
    mutable instructions: Instruction.t list;
    mutable next: Block.next;
  }
end

(* Break block id and continue block id for a loop *)
type loop_context = Block.id * Block.id

(* Immutable strings are deduplicated in MIR, and have a canonical value global and length global *)
module ImmutableString = struct
  type t = {
    value_global_val: Value.pointer_value;
    size_global_val: Value.pointer_value;
  }
end

type t = {
  pcx: Program_context.t;
  (* Data structures for MIR *)
  mutable main_label: label;
  mutable blocks: BlockBuilder.t IMap.t;
  mutable globals: Global.t SMap.t;
  mutable funcs: Function.t SMap.t;
  mutable types: Aggregate.t SMap.t;
  mutable current_block_builder: BlockBuilder.t option;
  (* Current function name and return type *)
  mutable current_func: label * Types.Type.t;
  mutable current_in_std_lib: bool;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
  (* Whether to filter out the standard library when dumping IR or asm *)
  filter_std_lib: bool;
  (* Variable decl loc to the var id of the pointer for the memory location it resides in during
     initial creation. These memory locations will be promoted to registers with phi nodes for joins. *)
  mutable variable_to_ptr_var_id: var_id LocMap.t;
  (* Function parameter decl loc to the var id for that parameter *)
  mutable param_to_var_id: var_id LocMap.t;
  (* Pointer var ids that shuld be promoted to registers during SSA pass *)
  mutable ptr_var_ids_to_ssaify: ISet.t;
  (* ADT signature id to its corresponding MIR layout *)
  mutable adt_sig_to_mir_layout: MirAdtLayout.t IMap.t;
  (* Trait signature id to its corresponding trait object vtables *)
  mutable trait_sig_to_trait_object_layout: MirTraitObjectLayout.t IMap.t;
  (* Names for all nongeneric functions that are pending generation *)
  mutable pending_nongeneric_funcs: SSet.t;
  (* All AST nodes for all globals that should be generated, indexed by their full name *)
  mutable pending_globals: Ast.Statement.VariableDeclaration.t SMap.t;
  (* All tuple types used in this program, keyed by their type arguments *)
  mutable tuple_instantiations: Aggregate.t TypeArgsHashtbl.t;
  (* All instances of generic functions used in this program that have been generated so far,
     uniquely identifier by the generic function name and its type arguments. *)
  mutable generic_func_instantiations: unit TypeArgsHashtbl.t SMap.t;
  (* All instances of generic functions that must still be generated. This must be empty for the
     emit pass to be complete. Keys are the generic function name and its type arguments, and
     value is a map of type parameter bindings to be used when generating the function instance. *)
  mutable pending_generic_func_instantiations: Types.Type.t IMap.t TypeArgsHashtbl.t SMap.t;
  (* Concrete types bound to type parameters in the current context. This is used when generating
     generic functions. *)
  mutable current_type_param_bindings: Types.Type.t IMap.t;
  (* All function declaration AST nodes, indexed by their full name *)
  mutable func_decl_nodes: Ast.Function.t SMap.t;
  (* All globals in program indexed by their decl loc *)
  mutable global_variable_decl_nodes: Ast.Statement.VariableDeclaration.t LocMap.t;
  (* Whether we are currently emitting blocks for the init function *)
  mutable in_init: bool;
  (* The last init block builder that was completed *)
  mutable last_init_block_builder: BlockBuilder.t option;
  mutable max_mutable_string_literal_id: int;
  mutable max_immutable_string_literal_id: int;
  (* Map from string to the deduplicated immutable string *)
  mutable immutable_string_literals: ImmutableString.t SMap.t;
}

let mk_aggregate ~ecx name loc elements =
  let aggregate = { Aggregate.id = mk_aggregate_id (); name; loc; elements } in
  ecx.types <- SMap.add name aggregate ecx.types;
  aggregate

(* Create a placeholder aggregate (during type instantiation) that will be filled in later *)
let mk_placeholder_aggregate ~ecx name loc = mk_aggregate ~ecx name loc []

let mk ~pcx =
  let ecx =
    {
      pcx;
      main_label = "";
      blocks = IMap.empty;
      globals = SMap.empty;
      funcs = SMap.empty;
      types = SMap.empty;
      current_block_builder = None;
      current_func = ("", Any);
      current_in_std_lib = false;
      current_loop_contexts = [];
      filter_std_lib =
        (Opts.dump_ir () || Opts.dump_pre_ssa_ir () || Opts.dump_virtual_asm () || Opts.dump_asm ())
        && not (Opts.dump_stdlib ());
      variable_to_ptr_var_id = LocMap.empty;
      param_to_var_id = LocMap.empty;
      ptr_var_ids_to_ssaify = ISet.empty;
      adt_sig_to_mir_layout = IMap.empty;
      trait_sig_to_trait_object_layout = IMap.empty;
      pending_nongeneric_funcs = SSet.empty;
      pending_globals = SMap.empty;
      tuple_instantiations = TypeArgsHashtbl.create 10;
      generic_func_instantiations = SMap.empty;
      pending_generic_func_instantiations = SMap.empty;
      current_type_param_bindings = IMap.empty;
      func_decl_nodes = SMap.empty;
      global_variable_decl_nodes = LocMap.empty;
      in_init = false;
      last_init_block_builder = None;
      max_mutable_string_literal_id = 0;
      max_immutable_string_literal_id = 0;
      immutable_string_literals = SMap.empty;
    }
  in
  (* Initialize long block *)
  ignore
    (mk_aggregate
       ~ecx
       Std_lib.std_long_long
       (Std_lib.lookup_stdlib_decl_loc Std_lib.std_long_long)
       [("$header", `LongT); ("value", `LongT)]);
  ecx

let builders_to_blocks builders =
  IMap.map
    (fun builder ->
      {
        Block.id = builder.BlockBuilder.id;
        instructions = List.rev builder.instructions;
        phis = [];
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
  | None -> failwith "No current block builder"
  | Some builder -> builder.instructions <- (mk_instr_id (), inst) :: builder.instructions

let mk_block_builder ~ecx =
  let block_id = mk_block_id () in
  let builder =
    { BlockBuilder.id = block_id; func = fst ecx.current_func; instructions = []; next = Halt }
  in
  ecx.blocks <- IMap.add block_id builder ecx.blocks;
  builder

let set_block_builder ~ecx builder = ecx.current_block_builder <- Some builder

let get_block_builder_throws ~ecx =
  match ecx.current_block_builder with
  | Some builder -> builder
  | None -> failwith "No current block builder"

let get_block_builder_id_throws ~ecx = (get_block_builder_throws ~ecx).id

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

let set_current_func ~ecx func return_ty = ecx.current_func <- (func, return_ty)

let push_loop_context ~ecx break_id continue_id =
  ecx.current_loop_contexts <- (break_id, continue_id) :: ecx.current_loop_contexts

let pop_loop_context ~ecx = ecx.current_loop_contexts <- List.tl ecx.current_loop_contexts

let get_loop_context ~ecx = List.hd ecx.current_loop_contexts

let get_ptr_var_id ~ecx use_loc =
  let decl_loc = Bindings.get_decl_loc_from_value_use ecx.pcx.bindings use_loc in
  match LocMap.find_opt decl_loc ecx.variable_to_ptr_var_id with
  | Some var_id -> var_id
  | None ->
    let var_id = mk_var_id () in
    ecx.variable_to_ptr_var_id <- LocMap.add decl_loc var_id ecx.variable_to_ptr_var_id;
    var_id

let add_function_param ~ecx decl_loc =
  let var_id = mk_var_id () in
  ecx.param_to_var_id <- LocMap.add decl_loc var_id ecx.param_to_var_id;
  var_id

let get_function_param_var_id ~ecx use_loc =
  let decl_loc = Bindings.get_decl_loc_from_value_use ecx.pcx.bindings use_loc in
  LocMap.find decl_loc ecx.param_to_var_id

let emit_init_section ~ecx f =
  let old_in_init = ecx.in_init in
  let old_func = ecx.current_func in
  ecx.in_init <- true;
  set_current_func ~ecx init_func_name Unit;

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
 * String literals
 *)

let add_mutable_string_literal ~ecx loc string =
  let name =
    let id = ecx.max_mutable_string_literal_id in
    ecx.max_mutable_string_literal_id <- id + 1;
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
  let global =
    { Global.loc; name; ty; init_val = Some (`ArrayStringL string); is_constant = true }
  in
  add_global ~ecx global;
  `PointerL (ty, name)

let add_immutable_string_literal ~ecx string =
  match SMap.find_opt string ecx.immutable_string_literals with
  (* Immutable strings have a single canonical global value and size *)
  | Some imm_string -> imm_string
  | None ->
    (* If not yet interned, generate new immutable string id *)
    let id = ecx.max_immutable_string_literal_id in
    ecx.max_immutable_string_literal_id <- id + 1;
    let size = String.length string in

    (* Create global for string value *)
    let value_name = ".IS" ^ string_of_int id in
    let value_ty = `ArrayT (`ByteT, size) in
    let value_global =
      {
        Global.loc = Loc.none;
        name = value_name;
        ty = value_ty;
        init_val = Some (`ArrayStringL string);
        is_constant = true;
      }
    in
    add_global ~ecx value_global;

    (* Create global for string size *)
    let size_name = value_name ^ "Size" in
    let size_global =
      {
        Global.loc = Loc.none;
        name = size_name;
        ty = `IntT;
        init_val = Some (`IntL (Int32.of_int size));
        is_constant = true;
      }
    in
    add_global ~ecx size_global;

    (* Intern globals for new immutable string *)
    let imm_string =
      {
        ImmutableString.value_global_val = `PointerL (value_ty, value_name);
        size_global_val = `PointerL (`IntT, size_name);
      }
    in
    ecx.immutable_string_literals <- SMap.add string imm_string ecx.immutable_string_literals;
    imm_string

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
      aggregate.elements <- elements;
      Aggregate aggregate
    ) else (
      (* If aggregate has only zero size elements then entire layout is zero size. Remove
         placeholder aggregate that was temporarily created. This is safe, meaning no other types
         can reference the placeholder aggregate, because if they did this would not have had only
         zero size elements. *)
      ecx.types <- SMap.remove parameterized_name ecx.types;
      set_layout ZeroSize;
      ZeroSize
    )
  | VariantsTemplate { tags; tag_mir_type; templates; _ } when SMap.is_empty templates ->
    PureEnum { tags; tag_mir_type }
  | VariantsTemplate { tags; tag_mir_type; templates; variant_locs } ->
    let parameterized_adt_name = mir_adt_layout.name ^ TypeArgs.to_string ~pcx:ecx.pcx type_args in
    let tag_element = mk_tag_element tag_mir_type in

    let union_aggregate = mk_placeholder_aggregate ~ecx parameterized_adt_name mir_adt_layout.loc in
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

    (* Find aligned layout and size of each data variant *)
    let (aligned_variant_elements, max_size) =
      SMap.fold
        (fun variant_name template (aligned_variant_elements, max_size) ->
          let elements = instantiate_mir_adt_template_elements ~ecx template type_param_bindings in
          (* Add tag element to start of aggregate, then align and insert padding *)
          let elements = tag_element :: elements in
          let ((_, size) as aligned_elements) = align_and_pad_aggregate_elements elements in
          (SMap.add variant_name aligned_elements aligned_variant_elements, max size max_size))
        templates
        (SMap.empty, 0)
    in
    layout.size <- max_size;

    (* Create aggregates for each data variant, inserting padding at end to max size *)
    layout.variants <-
      SMap.mapi
        (fun variant_name (elements, size) ->
          let aggregate_name = parameterized_adt_name ^ "::" ^ variant_name in
          let loc = SMap.find variant_name variant_locs in
          let elements = add_end_padding elements size max_size in
          mk_aggregate ~ecx aggregate_name loc elements)
        aligned_variant_elements;

    (* Create aggregates for union (and enum variants *)
    let tag_size = size_of_type tag_mir_type in
    union_aggregate.elements <- add_end_padding [tag_element] tag_size max_size;

    Variants layout

and instantiate_mir_adt_template_elements ~ecx template type_param_bindings =
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
        [("item", `PointerT `ByteT); ("vtable", `PointerT (`ArrayT (`FunctionT, vtable_size)))]
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
    (* Instantiate type's methods that are part of the trait object, building vtable array *)
    let (vtable_size, vtable_functions) =
      SMap.fold
        (fun method_name method_sig (i, vtable_functions) ->
          if Types.MethodSig.is_generic method_sig then
            (i, vtable_functions)
          else
            let method_value =
              get_method_function_value
                ~ecx
                ~method_name
                ~receiver_ty:ty
                ~method_instance_type_args:[]
            in
            (i + 1, method_value :: vtable_functions))
        trait_sig.methods
        (0, [])
    in
    let vtable_val = `ArrayVtableL (vtable_size, List.rev vtable_functions) in
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
    add_global
      ~ecx
      {
        Global.loc = adt_sig.loc;
        name = vtable_label;
        ty = vtable_mir_type;
        init_val = Some vtable_val;
        is_constant = true;
      };
    let vtable = `PointerL (vtable_mir_type, vtable_label) in

    (* Create aggregate type for type's trait object. Default to pointer field (for alignment) if
       type is zero width and has no MIR representation. *)
    let agg_label = Printf.sprintf "_object$%s$%s" full_adt_name full_trait_name in
    let mir_type =
      match to_mir_type ~ecx ty with
      | Some mir_type -> mir_type
      | None -> `PointerT (get_zero_size_type ~ecx)
    in
    let (agg_elements, _) =
      align_and_pad_aggregate_elements [("item", mir_type); ("vtable", `PointerT vtable_mir_type)]
    in
    let agg = mk_aggregate ~ecx agg_label adt_sig.loc agg_elements in

    let trait_object_instance = { MirTraitObjectLayout.vtable; agg } in
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
  | Bool -> Some `LongT
  | Byte -> Some `ByteT
  | Int -> Some `IntT
  | Long -> Some `LongT
  | IntLiteral { resolved; _ }
  | BoundedExistential { resolved; _ } ->
    to_mir_type ~ecx (Option.get resolved)
  | Function _ -> Some `FunctionT
  | Tuple elements ->
    instantiate_tuple ~ecx elements
    |> Option.map (fun tuple_agg -> `PointerT (`AggregateT tuple_agg))
  | ADT { adt_sig; type_args = [element_ty] } when adt_sig == !Std_lib.array_adt_sig ->
    let mir_type =
      match to_mir_type ~ecx element_ty with
      | Some mir_type -> mir_type
      | None -> get_zero_size_type ~ecx
    in
    Some (`PointerT mir_type)
  | ADT { adt_sig; type_args } ->
    let layout = get_mir_adt_layout ~ecx adt_sig type_args in
    (match layout with
    | Aggregate aggregate -> Some (`PointerT (`AggregateT aggregate))
    | Variants { union; _ } -> Some (`PointerT (`AggregateT union))
    | PureEnum { tag_mir_type; _ } -> Some (tag_mir_type :> Type.t)
    | InlineValue mir_type -> Some mir_type
    | ZeroSize -> None)
  | TraitObject { trait_sig; _ } ->
    let trait_object_layout = get_trait_object_layout ~ecx trait_sig in
    Some (`PointerT (`AggregateT trait_object_layout.trait_object_agg))
  | TypeParam _
  | TraitBound _
  | TVar _
  | Any ->
    failwith "Not allowed as value in IR"

and get_zero_size_type ~ecx =
  (* Add zero size type if it does not already exist *)
  match SMap.find_opt zero_size_name ecx.types with
  | None ->
    let agg = mk_placeholder_aggregate ~ecx zero_size_name Loc.none in
    `AggregateT agg
  | Some agg -> `AggregateT agg

and get_zero_size_global_pointer ~ecx =
  (* Add zero size global if it does not already exist *)
  let zero_size_type = get_zero_size_type ~ecx in
  if not (SMap.mem zero_size_name ecx.globals) then
    add_global
      ~ecx
      {
        Global.loc = Loc.none;
        name = zero_size_name;
        ty = zero_size_type;
        init_val = None;
        is_constant = false;
      };
  `PointerL (zero_size_type, zero_size_name)

(*
 * Nongeneric Functions
 *)
and get_nongeneric_function_value ~ecx name : Value.function_value =
  (* Mark function as pending if it has not yet been generated *)
  (match SMap.find_opt name ecx.funcs with
  | None -> ecx.pending_nongeneric_funcs <- SSet.add name ecx.pending_nongeneric_funcs
  | Some _ -> ());
  `FunctionL name

and pop_pending_nongeneric_function ~ecx =
  match SSet.choose_opt ecx.pending_nongeneric_funcs with
  | None -> None
  | Some name ->
    let func_decl = SMap.find name ecx.func_decl_nodes in
    Some (name, func_decl)

and mark_pending_nongeneric_function_completed ~ecx name =
  ecx.pending_nongeneric_funcs <- SSet.remove name ecx.pending_nongeneric_funcs

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

and get_generic_function_value ~ecx name key_type_params key_type_args : Value.function_value =
  let key_type_args = List.map (find_rep_non_generic_type ~ecx) key_type_args in
  let name_with_args =
    let type_args_string = TypeArgs.to_string ~pcx:ecx.pcx key_type_args in
    Printf.sprintf "%s%s" name type_args_string
  in
  let already_instantiated =
    match SMap.find_opt name ecx.generic_func_instantiations with
    | None -> false
    | Some instantiated_type_args ->
      (match TypeArgsHashtbl.find_opt instantiated_type_args key_type_args with
      | None -> false
      | Some _ -> true)
  in
  ( if already_instantiated then
    ()
  else
    let pending_type_args =
      match SMap.find_opt name ecx.pending_generic_func_instantiations with
      | None -> TypeArgsHashtbl.create 2
      | Some pending_instantiation_type_args -> pending_instantiation_type_args
    in
    match TypeArgsHashtbl.find_opt pending_type_args key_type_args with
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
      TypeArgsHashtbl.add pending_type_args key_type_args instantiated_type_param_bindings;
      ecx.pending_generic_func_instantiations <-
        SMap.add name pending_type_args ecx.pending_generic_func_instantiations );
  `FunctionL name_with_args

and get_method_function_value
    ~ecx
    ~(method_name : string)
    ~(receiver_ty : Types.Type.t)
    ~(method_instance_type_args : Types.Type.t list) : Value.function_value =
  let (receiver_adt_sig, receiver_type_args) =
    match receiver_ty with
    | ADT { adt_sig; type_args; _ } -> (adt_sig, type_args)
    | _ -> (Std_lib.get_primitive_adt_sig receiver_ty, [])
  in

  (* Find method, along with the trait it was defined in *)
  let method_sig = Types.AdtSig.lookup_method receiver_adt_sig method_name in
  let method_sig = Option.get method_sig in
  let source_trait_instance = method_sig.source_trait_instance in
  let source_trait_sig = source_trait_instance.trait_sig in

  (* Calculate generic keys from trait/type *)
  let (extra_key_type_params, extra_key_type_args) =
    if method_sig.trait_sig == source_trait_sig then
      (* Method was directly declared on the ADT's trait. Substitute type args for trait's type args *)
      (source_trait_sig.type_params, receiver_type_args)
    else
      (* Method was declared on a trait that the ADT implements. The source trait instance's type
         args are in terms of the ADT trait's type params, so first map from the ADT trait's type
         params to the actual ADT instance's type args in the source trait instance's type args.
         The key args are these mapped source trait instance's type args along with the a `this`
         type of the ADT instance. *)
      let bindings =
        Types.bind_type_params_to_args method_sig.trait_sig.type_params receiver_type_args
      in
      let type_args =
        List.map (Types.substitute_type_params bindings) source_trait_instance.type_args
      in
      let all_key_type_params = source_trait_sig.this_type_param :: source_trait_sig.type_params in
      let all_key_type_args = receiver_ty :: type_args in
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
    TypeArgsHashtbl.add instantiations type_args ();

    let name_with_args =
      let type_args_string = TypeArgs.to_string ~pcx:ecx.pcx type_args in
      Printf.sprintf "%s%s" name type_args_string
    in
    Some (name, name_with_args, type_param_bindings)

(*
 * Global Variables
 *)

(*
 * Return the pointer literal value for a given global variable if the variable exists in MIR.
 *
 * If global variable has zero size then instead return None.
 *)
let get_global_pointer ~ecx binding =
  let loc = binding.Bindings.ValueBinding.loc in
  let name = mk_value_binding_name binding in
  match SMap.find_opt name ecx.globals with
  (* Global will be in MIR globals map if it has already been created or is pending *)
  | Some global -> Some (`PointerL (global.ty, global.name))
  | None ->
    let var_decl = Bindings.get_var_decl binding in
    (match to_mir_type ~ecx (find_rep_non_generic_type ~ecx (TVar var_decl.tvar)) with
    (* Global does not exist if it has zero size type *)
    | None -> None
    | Some ty ->
      (* Add global to pending globals queue *)
      let decl_node = LocMap.find loc ecx.global_variable_decl_nodes in
      ecx.pending_globals <- SMap.add name decl_node ecx.pending_globals;

      (* Add global to MIR globals map *)
      add_global
        ~ecx
        { Global.loc; name; ty; init_val = None; is_constant = decl_node.kind = Immutable };

      Some (`PointerL (ty, name)))

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

let add_global_variable_declaration_node ~ecx loc decl =
  ecx.global_variable_decl_nodes <- LocMap.add loc decl ecx.global_variable_decl_nodes
