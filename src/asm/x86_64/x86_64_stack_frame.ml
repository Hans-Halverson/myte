open Asm
open Asm_register
open X86_64_builders
open X86_64_gen_context
open X86_64_register

let func_should_save_base_pointer _func = false

let func_has_stack_frame func = func.Function.num_stack_frame_slots <> 0

let func_stack_frame_size func = func.Function.num_stack_frame_slots * 8

let mk_precolored_sp () = mk_precolored ~type_:Long `SP

let mk_precolored_bp () = mk_precolored ~type_:Long `BP

(* Write all function prologues by saving the base pointer, pushing the used callee saved registers
   on the stack, and allocating a stack frame when applicable. *)
let write_function_prologues ~(gcx : Gcx.t) =
  FunctionSet.iter
    (fun func ->
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
        (fun reg ->
          if RegSet.mem reg func.spilled_callee_saved_regs then
            add_instr `PushM [| mk_precolored ~type_:Long reg |])
        callee_saved_registers;

      (* Allocate space for the stack frame *)
      if func_has_stack_frame func then
        let stack_frame_size = func_stack_frame_size func in
        let stack_frame_size_imm = mk_imm ~imm:(Imm32 (Int32.of_int stack_frame_size)) in
        add_instr (`SubIM Size64) [| stack_frame_size_imm; mk_precolored_sp () |])
    gcx.funcs

(* Write all function epilogues by destroying the stack frame, popping used callee saved registers
   off the sack, and restoring the base pointer when applicable. *)
let write_function_epilogues ~(gcx : Gcx.t) =
  funcs_iter_blocks gcx.funcs (fun block ->
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
            (if func_has_stack_frame func then
              let stack_frame_size = func_stack_frame_size func in
              let stack_frame_size_imm = mk_imm ~imm:(Imm32 (Int32.of_int stack_frame_size)) in
              add_instr (`AddIM Size64) [| stack_frame_size_imm; mk_precolored_sp () |]);

            (* Pop all saved callee saved registers off the stack. Collect into accumulator first
               so that instructions can be added in reverse order. *)
            let pop_instrs =
              RegSet.fold
                (fun reg acc ->
                  if RegSet.mem reg func.spilled_callee_saved_regs then (
                    offset := !offset + 1;
                    (`PopM, [| mk_precolored ~type_:Long reg |]) :: acc
                  ) else
                    acc)
                callee_saved_registers
                []
            in
            List.iter (fun (instr, operand) -> add_instr instr operand) pop_instrs;

            (* Restore the saved base pointer *)
            if func_should_save_base_pointer func then (
              add_instr (`MovMM Size64) [| mk_precolored_bp (); mk_precolored_sp () |];
              add_instr `PopM [| mk_precolored_bp () |]
            )
          | _ -> ()))
