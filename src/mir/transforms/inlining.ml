open Basic_collections
open Mir
open Mir_builders

(* Cost below which functions will be inlined. Cost is number of additional instructions introduced
   by inlining. *)
let inline_threshold = 1000

(* Functions with fewer than this number of instructions will always be inlined *)
let always_inline_num_instructions_threshold = 20

class inlining_mapper ~(* Caller which is being inlined into *)
                      (caller_func : Function.t) =
  object
    (* Map from value in inlined function to the matching value in the caller function *)
    val mutable value_map : Value.t VMap.t = VMap.empty

    (* Map from block in inlined function to the matching block in the caller function *)
    val mutable block_map : Block.t BlockMap.t = BlockMap.empty

    method add_value_mapping v1 v2 = value_map <- VMap.add v1 v2 value_map

    method map_value value =
      match value.Value.value with
      | Lit _ -> value
      | Instr _
      | Argument _ ->
        (match VMap.find_opt value value_map with
        | Some mapped_value -> mapped_value
        | None ->
          let mapped_value = mk_uninit_value () in
          (* Temporarily use old value, this will be overwritten but is necessary to pass type checks
             on this value before it is filled. *)
          mapped_value.value <- value.value;
          value_map <- VMap.add value mapped_value value_map;
          mapped_value)

    method map_block block =
      match BlockMap.find_opt block block_map with
      | Some mapped_block -> mapped_block
      | None ->
        let mapped_block = mk_block ~func:caller_func in
        block_map <- BlockMap.add block mapped_block block_map;
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

  let mapper = new inlining_mapper ~caller_func in

  (* Map parameters to call arguments during inlining of this call *)
  let { Instruction.Call.args; _ } =
    match call_instr.instr with
    | Call call -> call
    | _ -> failwith "Expected call instruction"
  in
  List.iter2
    (fun param arg_use -> mapper#add_value_mapping param arg_use.Use.value)
    func.params
    args;

  (* First block continues to start of inlined body *)
  let inlined_func_start_block = mapper#map_block func.start_block in
  mk_continue_ ~block:caller_before_block ~continue:inlined_func_start_block;
  add_block_link caller_before_block inlined_func_start_block;

  (* Copy all blocks in the body of the inlined function into caller *)
  func_iter_blocks func (fun block ->
      iter_instructions block (fun instr_value instr ->
          let new_block = mapper#map_block block in
          let new_value = mapper#map_value instr_value in
          (match instr.instr with
          (* Returns replaced with continue to join block, adding return argument to join block phi *)
          | Ret arg ->
            (match arg with
            | None -> ()
            | Some arg ->
              let arg = mapper#map_value arg.Use.value in
              let phi_val = Option.get ret_phi_instr in
              let phi = cast_to_phi (cast_to_instruction phi_val) in
              phi_add_arg ~phi_val ~phi ~block:new_block ~value:arg);
            set_continue_instr ~value:new_value ~continue:caller_after_block;
            add_block_link new_block caller_after_block
          (* All other instructions are directly copied, with values and blocks mapped s*)
          | _ -> Mir_mapper.map_instruction ~mapper instr_value);
          append_instruction new_block new_value))
