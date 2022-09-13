open Basic_collections
open X86_64_builders
open X86_64_gen_context
open X86_64_register
open X86_64_instructions

let func_should_save_base_pointer _func = false

let func_has_stack_frame func = func.Function.num_stack_frame_slots <> 0

let func_stack_frame_size func = func.Function.num_stack_frame_slots * 8

let mk_precolored_sp () = mk_precolored ~type_:Long SP

let mk_precolored_bp () = mk_precolored ~type_:Long BP

(* Write all function prologues by saving the base pointer, pushing the used callee saved registers
   on the stack, and allocating a stack frame when applicable. *)
let write_function_prologues ~(gcx : Gcx.t) =
  let open Instruction in
  FunctionSet.iter
    (fun func ->
      let prologue = IMap.find func.Function.prologue gcx.blocks_by_id in

      (* Only need to save the base pointer if there is data on stack *)
      let save_base_pointer_instrs =
        if func_should_save_base_pointer func then
          [
            mk_instr (PushM (mk_precolored_bp ()));
            mk_instr (MovMM (Size64, mk_precolored_sp (), mk_precolored_bp ()));
          ]
        else
          []
      in
      (* Push all used callee saved registers onto the stack *)
      let push_instrs =
        RegSet.fold
          (fun reg acc ->
            if RegSet.mem reg func.spilled_callee_saved_regs then
              mk_instr (PushM (mk_precolored ~type_:Long reg)) :: acc
            else
              acc)
          callee_saved_registers
          []
      in
      (* Allocate space for the stack frame *)
      let allocate_stack_frame_instrs =
        if func_has_stack_frame func then
          let stack_frame_size = func_stack_frame_size func in
          [mk_instr (SubIM (Size64, Imm32 (Int32.of_int stack_frame_size), mk_precolored_sp ()))]
        else
          []
      in
      prologue.instructions <-
        save_base_pointer_instrs
        @ List.rev push_instrs
        @ allocate_stack_frame_instrs
        @ prologue.instructions)
    gcx.funcs

(* Write all function epilogues by destroying the stack frame, popping used callee saved registers
   off the sack, and restoring the base pointer when applicable. *)
let write_function_epilogues ~(gcx : Gcx.t) =
  IMap.iter
    (fun _ block ->
      let open Block in
      let offset = ref 0 in
      block.instructions <-
        List.map
          (fun instr ->
            let open Instruction in
            match instr.instr with
            | Ret ->
              let func = block.func in

              (* Destroy the stack frame *)
              let destroy_stack_frame_instrs =
                if func_has_stack_frame func then
                  let stack_frame_size = func_stack_frame_size func in
                  [
                    mk_instr
                      (AddIM (Size64, Imm32 (Int32.of_int stack_frame_size), mk_precolored_sp ()));
                  ]
                else
                  []
              in
              (* Pop all saved callee saved registers off the stack *)
              let pop_instrs =
                RegSet.fold
                  (fun reg acc ->
                    if RegSet.mem reg func.spilled_callee_saved_regs then (
                      offset := !offset + 1;
                      mk_instr (PopM (mk_precolored ~type_:Long reg)) :: acc
                    ) else
                      acc)
                  callee_saved_registers
                  []
              in
              (* Restore the saved base pointer *)
              let restore_base_pointer_instrs =
                if func_should_save_base_pointer func then
                  [
                    mk_instr (MovMM (Size64, mk_precolored_bp (), mk_precolored_sp ()));
                    mk_instr (PopM (mk_precolored_bp ()));
                  ]
                else
                  []
              in
              destroy_stack_frame_instrs @ pop_instrs @ restore_base_pointer_instrs @ [instr]
            | _ -> [instr])
          block.instructions
        |> List.flatten)
    gcx.blocks_by_id
