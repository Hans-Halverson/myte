open Asm
open Asm_builders
open Asm_register
open X86_64_gen_context

(* Size of the red zone in bytes *)
let red_zone_size = 128

let func_should_save_base_pointer _func = false

let func_has_stack_frame func = func.Function.num_stack_frame_slots <> 0

let func_stack_frame_size func = func.Function.num_stack_frame_slots * 8

let func_stack_frame_uses_red_zone func = func.Function.is_leaf && func_stack_frame_size func <= 128

let mk_precolored_sp () = mk_precolored ~type_:Long `SP

let mk_precolored_bp () = mk_precolored ~type_:Long `BP

let resolve_to_physical_stack_slot operand offset =
  operand.Operand.value <-
    X86_64_MemoryAddress
      {
        base = RegBase (mk_precolored ~type_:Long `SP);
        offset = Some (ImmediateOffset (Int32.of_int offset));
        index_and_scale = None;
      }

(* Write function prologue by saving the base pointer, pushing the used callee saved registers
   on the stack, and allocating a stack frame when applicable. *)
let write_function_prologue func =
  (* All new instructions added to start of prologue *)
  let prologue = func.Function.prologue in
  let prologue_first_instr = get_first_instr_opt prologue in
  let add_instr instr operands =
    let instr = mk_blockless_instr instr operands in
    match prologue_first_instr with
    | None -> append_instruction prologue instr
    | Some first_instr -> insert_instruction_before ~before:first_instr instr
  in

  (* Only need to save the base pointer if there is data on stack *)
  if func_should_save_base_pointer func then (
    add_instr `PushM [| mk_precolored_bp () |];
    add_instr (`MovMM Size64) [| mk_precolored_sp (); mk_precolored_bp () |]
  );

  (* Push all used callee saved registers onto the stack *)
  RegSet.iter
    (fun reg -> add_instr `PushM [| mk_precolored ~type_:Long reg |])
    func.spilled_callee_saved_regs;

  (* Allocate space for the stack frame *)
  if func_has_stack_frame func && not (func_stack_frame_uses_red_zone func) then
    let stack_frame_size = func_stack_frame_size func in
    let stack_frame_size_imm = mk_imm ~imm:(Imm32 (Int32.of_int stack_frame_size)) in
    add_instr (`SubIM Size64) [| stack_frame_size_imm; mk_precolored_sp () |]

(* Write function epilogue by destroying the stack frame, popping used callee saved registers
   off the sack, and restoring the base pointer when applicable. *)
let write_function_epilogue func =
  func_iter_blocks func (fun block ->
      let open Block in
      let offset = ref 0 in
      iter_instructions block (fun instr ->
          match instr.instr with
          | `Ret ->
            let func = block.func in
            let ret_instr = instr in
            let add_instr instr operands =
              let instr = mk_blockless_instr instr operands in
              insert_instruction_before ~before:ret_instr instr
            in

            (* Destroy the stack frame *)
            (if func_has_stack_frame func && not (func_stack_frame_uses_red_zone func) then
              let stack_frame_size = func_stack_frame_size func in
              let stack_frame_size_imm = mk_imm ~imm:(Imm32 (Int32.of_int stack_frame_size)) in
              add_instr (`AddIM Size64) [| stack_frame_size_imm; mk_precolored_sp () |]);

            (* Pop all saved callee saved registers off the stack. Collect into accumulator first
               so that instructions can be added in reverse order. *)
            let pop_instrs =
              RegSet.fold
                (fun reg acc ->
                  offset := !offset + 1;
                  (`PopM, [| mk_precolored ~type_:Long reg |]) :: acc)
                func.spilled_callee_saved_regs
                []
            in
            List.iter (fun (instr, operand) -> add_instr instr operand) pop_instrs;

            (* Restore the saved base pointer *)
            if func_should_save_base_pointer func then (
              add_instr (`MovMM Size64) [| mk_precolored_bp (); mk_precolored_sp () |];
              add_instr `PopM [| mk_precolored_bp () |]
            )
          | _ -> ()))

let write_stack_slots (func : Function.t) =
  let adjust_offset =
    if func_stack_frame_uses_red_zone func then
      let stack_frame_size = func_stack_frame_size func in
      (fun offset -> offset - stack_frame_size)
    else
      Function_utils.id
  in

  (* Write physical addresses in stack for each argument stack slot in function *)
  List.iter
    (fun stack_slot_op ->
      let i = cast_to_stack_slot stack_slot_op in
      resolve_to_physical_stack_slot stack_slot_op (adjust_offset (i * 8)))
    func.argument_stack_slots;

  (* Write physical addresses in stack for each vslot in function *)
  OperandSet.iter
    (fun stack_slot_op ->
      let i = cast_to_stack_slot stack_slot_op in
      resolve_to_physical_stack_slot stack_slot_op (adjust_offset (i * 8)))
    func.spilled_vslots;

  (* Write physical addresses for each param passed on stack now that stack frame size is known *)
  let num_used_callee_saved_regs = RegSet.cardinal func.spilled_callee_saved_regs in
  List.iter
    (fun param_op ->
      match param_op.Operand.value with
      | StackSlot i ->
        (* Offset must reach past entire stack frame, then all used callee saved registers that were
            pushed on stack, then return address pushed onto stack from call instruction, and then
            finally can start accessing function arguments that were pushed onto stack. *)
        let offset = (func.num_stack_frame_slots + num_used_callee_saved_regs - i) * 8 in
        resolve_to_physical_stack_slot param_op (adjust_offset offset)
      | _ -> ())
    func.params

let write_stack_frame ~(gcx : Gcx.t) =
  FunctionSet.iter
    (fun func ->
      write_function_prologue func;
      write_function_epilogue func;
      write_stack_slots func)
    gcx.funcs
