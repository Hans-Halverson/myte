open Basic_collections
open Mir
open Mir_builders
open Mir_type

(* Cost below which functions will be inlined. Cost is number of additional instructions introduced
   by inlining. *)
let inline_threshold = 1000

(* Functions with fewer than this number of instructions will always be inlined *)
let always_inline_num_instructions_threshold = 20

module InlineContext = struct
  type t = {
    (* Caller which is being inlined into *)
    caller_func: Function.t;
        (* Block in the caller function that all ret functions in inlined functions jump to. Phi
           instruction with all returned values will be inserted at the start of this block. *)
    ret_join_block: Block.t;
    ret_phi_instr: Value.t option;
    (* Map from value in inlined function to the matching value in the caller function *)
    mutable value_map: Value.t VMap.t;
    (* Map from block in inlined function to the matching block in the caller function *)
    mutable block_map: Block.t BlockMap.t;
  }

  let mk ~caller_func ~ret_join_block ~ret_phi_instr =
    {
      caller_func;
      ret_join_block;
      ret_phi_instr;
      value_map = VMap.empty;
      block_map = BlockMap.empty;
    }

  let add_value_mapping ~cx v1 v2 = cx.value_map <- VMap.add v1 v2 cx.value_map

  let get_mapped_value ~cx value =
    match value.Value.value with
    | Lit _ -> value
    | Instr _
    | Argument _ ->
      (match VMap.find_opt value cx.value_map with
      | Some mapped_value -> mapped_value
      | None ->
        let mapped_value = mk_uninit_value () in
        (* Temporarily use old value, this will be overwritten but is necessary to pass type checks
           on this value before it is filled. *)
        mapped_value.value <- value.value;
        cx.value_map <- VMap.add value mapped_value cx.value_map;
        mapped_value)

  let get_mapped_block ~cx block =
    match BlockMap.find_opt block cx.block_map with
    | Some mapped_block -> mapped_block
    | None ->
      let mapped_block = mk_block ~func:cx.caller_func in
      cx.block_map <- BlockMap.add block mapped_block cx.block_map;
      mapped_block
end

let rec run ~(program : Program.t) ~(pcx : Program_context.t) =
  (* Find all function roots. Normally this is just the main function as only functions reachable
     from main are generated, but if emitting all functions we must find unused functions. *)
  let func_roots =
    if Opts.emit_all () then
      SMap.fold
        (fun _ func roots ->
          if value_has_uses func.Function.value then
            roots
          else
            func :: roots)
        program.funcs
        []
    else
      let std_sys_init_func = SMap.find Std_lib.std_sys_init program.funcs in
      [program.main_func; std_sys_init_func]
  in
  let ordered_funcs = Mir_graph_ordering.get_ordered_call_graph func_roots in
  List.iter
    (fun func ->
      let inlinable_call_instrs = get_inlinable_call_instrs func in
      if should_inline_function ~program ~pcx func inlinable_call_instrs then
        inline_function ~program func inlinable_call_instrs)
    ordered_funcs

and should_inline_function
    ~(program : Program.t)
    ~(pcx : Program_context.t)
    (func : Function.t)
    (inlinable_call_instrs : Value.t list) =
  func != program.main_func
  && func.name != Std_lib.std_sys_init
  && inlinable_call_instrs != []
  && (not (Attributes.has_no_inline ~store:pcx.attribute_store func.loc))
  && (Attributes.has_inline ~store:pcx.attribute_store func.loc
     || (Opts.optimize () && calculate_inline_cost func inlinable_call_instrs < inline_threshold))

and calculate_inline_cost (func : Function.t) (inlinable_call_instrs : Value.t list) : int =
  let num_inlinable_call_instrs = List.length inlinable_call_instrs in
  if num_inlinable_call_instrs == 1 then
    0
  else
    let num_instructions = ref 0 in
    func_iter_blocks func (fun block ->
        iter_instructions block (fun _ _ -> num_instructions := !num_instructions + 1));
    if !num_instructions <= always_inline_num_instructions_threshold then
      0
    else
      !num_instructions * num_inlinable_call_instrs

and get_inlinable_call_instrs (func : Function.t) : Value.t list =
  let call_instrs = ref [] in
  value_iter_uses ~value:func.value (fun func_use ->
      match func_use.Use.user.value with
      | Instr { instr = Call { func = Value use; _ }; block = { func = caller_in_func; _ }; _ } ->
        (match use.value.value with
        | Lit (Function func_lit)
        (* Only inline when it is the called function, not when function is an argument to a call.
           Do not inline recursive calls. *)
          when Function.equal func_lit func && not (Function.equal caller_in_func func) ->
          call_instrs := func_use.user :: !call_instrs
        | _ -> ())
      | _ -> ());
  !call_instrs

and inline_function
    ~(program : Program.t) (func : Function.t) (inlinable_call_instrs : Value.t list) =
  List.iter (inline_function_at_callsite func) inlinable_call_instrs;

  (* Remove function if it has no uses remaining *)
  if not (value_has_uses func.value) then program_remove_func ~program ~func

and inline_function_at_callsite (func : Function.t) (call_instr_value : Value.t) =
  let call_instr = cast_to_instruction call_instr_value in
  let caller_func = call_instr.block.func in

  (* Split block around call instruction, removing call instruction *)
  let (caller_before_block, caller_after_block) = split_block_after_instruction call_instr_value in
  (* If function returns a value, second block starts with a phi that joins results all returns *)
  let ret_phi_instr =
    match func.return_type with
    | None -> None
    | Some type_ ->
      let phi_value = mk_blockless_phi ~type_ ~args:BlockMap.empty in
      prepend_instruction caller_after_block phi_value;
      value_replace_uses ~from:call_instr_value ~to_:phi_value;
      Some phi_value
  in
  remove_instruction call_instr_value;

  let cx = InlineContext.mk ~caller_func ~ret_join_block:caller_after_block ~ret_phi_instr in

  (* Map parameters to call arguments during inlining of this call *)
  let { Instruction.Call.args; _ } =
    match call_instr.instr with
    | Call call -> call
    | _ -> failwith "Expected call instruction"
  in
  List.iter2
    (fun param arg_use -> InlineContext.add_value_mapping ~cx param arg_use.Use.value)
    func.params
    args;

  (* First block continues to start of inlined body *)
  let inlined_func_start_block = InlineContext.get_mapped_block ~cx func.start_block in
  mk_continue_ ~block:caller_before_block ~continue:inlined_func_start_block;
  add_block_link caller_before_block inlined_func_start_block;

  (* Copy all blocks in the body of the inlined function into caller *)
  func_iter_blocks func (fun block -> map_block ~cx block)

and map_block ~(cx : InlineContext.t) (source_block : Block.t) =
  let map_block block = InlineContext.get_mapped_block ~cx block in
  let map_value value = InlineContext.get_mapped_value ~cx value in
  let map_use use = map_value use.Use.value in

  let block = InlineContext.get_mapped_block ~cx source_block in
  iter_instructions source_block (fun instr_value instr ->
      let type_ = instr.type_ in
      let value = map_value instr_value in
      (match instr.instr with
      (* Returns replaced with continue to join block, adding return argument to join block phi *)
      | Ret arg ->
        (match arg with
        | None -> ()
        | Some arg ->
          let arg = map_use arg in
          let phi_val = Option.get cx.ret_phi_instr in
          let phi = cast_to_phi (cast_to_instruction phi_val) in
          phi_add_arg ~phi_val ~phi ~block:instr.block ~value:arg);
        set_continue_instr ~value ~continue:cx.ret_join_block;
        add_block_link block cx.ret_join_block
      (* All other instructions are directly copied, with values and blocks mapped s*)
      | Phi { args } ->
        let args =
          BlockMap.fold
            (fun source_block source_use args ->
              BlockMap.add (map_block source_block) (map_use source_use) args)
            args
            BlockMap.empty
        in
        set_phi_instr ~value ~type_ ~args
      | Call { func; args; has_return } ->
        let args = List.map map_use args in
        let return =
          if has_return then
            Some type_
          else
            None
        in
        (match func with
        | Value func_use ->
          let func = map_use func_use in
          set_call_instr ~value ~func ~args ~return
        | MirBuiltin builtin -> set_call_builtin_instr ~value ~builtin ~args ~return)
      | StackAlloc type_ -> set_stack_alloc_instr ~value ~type_
      | Load ptr ->
        let ptr = map_use ptr in
        set_load_instr ~value ~ptr
      | Store (ptr, stored_value) ->
        let ptr = map_use ptr in
        let stored_value = map_use stored_value in
        set_store_instr ~instr_value:value ~ptr ~stored_value
      | GetPointer { pointer; pointer_offset; offsets } ->
        let pointer = map_use pointer in
        let pointer_offset = Option.map map_use pointer_offset in
        let offsets =
          List.map
            (fun offset ->
              let open Instruction.GetPointer in
              match offset with
              | PointerIndex index -> PointerIndex (map_use index)
              | FieldIndex index -> FieldIndex index)
            offsets
        in
        let type_ = cast_to_pointer_type type_ in
        set_get_pointer_instr ~value ~type_ ~ptr:pointer ~pointer_offset ~offsets ()
      | Unary (op, arg) ->
        let arg = map_use arg in
        set_unary_instr ~value ~op ~arg
      | Binary (op, left, right) ->
        let left = map_use left in
        let right = map_use right in
        set_binary_instr ~value ~op ~left ~right
      | Cmp (cmp, left, right) ->
        let left = map_use left in
        let right = map_use right in
        set_cmp_instr ~value ~cmp ~left ~right
      | Cast arg ->
        let arg = map_use arg in
        set_cast_instr ~value ~arg ~type_
      | Trunc arg ->
        let arg = map_use arg in
        set_trunc_instr ~value ~arg ~type_
      | SExt arg ->
        let arg = map_use arg in
        set_sext_instr ~value ~arg ~type_
      | ZExt arg ->
        let arg = map_use arg in
        set_zext_instr ~value ~arg ~type_
      | IntToFloat arg ->
        let arg = map_use arg in
        set_int_to_float_instr ~value ~arg ~type_
      | FloatToInt arg ->
        let arg = map_use arg in
        set_float_to_int_instr ~value ~arg ~type_
      | Unreachable -> set_unreachable_instr ~value
      | Continue continue ->
        let continue = map_block continue in
        set_continue_instr ~value ~continue;
        add_block_link block continue
      | Branch { test; continue; jump } ->
        let test = map_use test in
        let continue = map_block continue in
        let jump = map_block jump in
        set_branch_instr ~value ~test ~continue ~jump;
        add_block_link block continue;
        add_block_link block jump
      | Mov arg ->
        let arg = map_use arg in
        set_mov_instr ~value ~arg);

      append_instruction block value)
