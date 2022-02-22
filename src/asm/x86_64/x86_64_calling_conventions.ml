open Basic_collections
open X86_64_builders
open X86_64_gen_context
open X86_64_instructions

let func_should_save_base_pointer _func = false

let func_has_stack_frame func = func.Function.num_stack_frame_slots <> 0

let func_stack_frame_size func = func.Function.num_stack_frame_slots * 8

(* Write all function prologues by saving the base pointer, pushing the used callee saved registers
   on the stack, and allocating a stack frame when applicable. *)
let write_function_prologues ~(gcx : Gcx.t) =
  let open Instruction in
  IMap.iter
    (fun _ func ->
      let prologue = IMap.find func.Function.prologue gcx.blocks_by_id in
      (* Only need to save the base pointer if there is data on stack *)
      let save_base_pointer_instrs =
        if func_should_save_base_pointer func then
          [
            (Gcx.mk_instr_id_for_block ~gcx prologue, PushM (mk_precolored BP));
            ( Gcx.mk_instr_id_for_block ~gcx prologue,
              MovMM (Size64, mk_precolored SP, mk_precolored BP) );
          ]
        else
          []
      in
      (* Push all used callee saved registers onto the stack *)
      let push_instrs =
        RegSet.fold
          (fun reg acc ->
            if RegSet.mem reg func.spilled_callee_saved_regs then
              Instruction.(mk_id (), PushM (mk_precolored reg)) :: acc
            else
              acc)
          callee_saved_registers
          []
      in
      (* Allocate space for the stack frame *)
      let allocate_stack_frame_instrs =
        if func_has_stack_frame func then
          let stack_frame_size = func_stack_frame_size func in
          [
            ( Gcx.mk_instr_id_for_block ~gcx prologue,
              SubIM (Size64, Imm32 (Int32.of_int stack_frame_size), mk_precolored SP) );
          ]
        else
          []
      in
      prologue.instructions <-
        save_base_pointer_instrs
        @ List.rev push_instrs
        @ allocate_stack_frame_instrs
        @ prologue.instructions)
    gcx.funcs_by_id

(* Write all function epilogues by destroying the stack frame, popping used callee saved registers
   off the sack, and restoring the base pointer when applicable. *)
let write_function_epilogues ~(gcx : Gcx.t) =
  IMap.iter
    (fun _ block ->
      let open Block in
      let func = IMap.find block.func gcx.funcs_by_id in
      let offset = ref 0 in
      block.instructions <-
        List.map
          (fun ((_, instr) as instr_with_id) ->
            let open Instruction in
            match instr with
            | Ret ->
              (* Destroy the stack frame *)
              let destroy_stack_frame_instrs =
                if func_has_stack_frame func then
                  let stack_frame_size = func_stack_frame_size func in
                  [
                    ( Gcx.mk_instr_id_for_block ~gcx block,
                      AddIM (Size64, Imm32 (Int32.of_int stack_frame_size), mk_precolored SP) );
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
                      Instruction.(Gcx.mk_instr_id_for_block ~gcx block, PopM (mk_precolored reg))
                      :: acc
                    ) else
                      acc)
                  callee_saved_registers
                  []
              in
              (* Restore the saved base pointer *)
              let restore_base_pointer_instrs =
                if func_should_save_base_pointer func then
                  [
                    ( Gcx.mk_instr_id_for_block ~gcx block,
                      MovMM (Size64, mk_precolored BP, mk_precolored SP) );
                    (Gcx.mk_instr_id_for_block ~gcx block, PopM (mk_precolored BP));
                  ]
                else
                  []
              in
              destroy_stack_frame_instrs
              @ pop_instrs
              @ restore_base_pointer_instrs
              @ [instr_with_id]
            | _ -> [instr_with_id])
          block.instructions
        |> List.flatten)
    gcx.blocks_by_id
