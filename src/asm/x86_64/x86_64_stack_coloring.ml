open Asm
open Asm_instruction_definition
open X86_64_builders
open X86_64_gen_context

class stack_coloring =
  object
    inherit Asm_stack_coloring.stack_coloring

    method calculate_live_out func =
      let liveness_analyzer = new X86_64_liveness_analysis.vslots_liveness_analyzer func in
      let (_, live_out) = liveness_analyzer#analyze () in
      live_out

    method find_vslot_use_defs instr =
      let uses = ref OperandSet.empty in
      let defs = ref OperandSet.empty in

      instr_iter_reg_mem_operands instr (fun operand operand_def ->
          match operand.value with
          | VirtualStackSlot ->
            if operand_is_use operand_def then uses := OperandSet.add operand !uses;
            if operand_is_def operand_def then defs := OperandSet.add operand !defs
          | _ -> ());

      (!uses, !defs)
  end

let stack_colorer = new stack_coloring

let color_stack_slots ~(gcx : Gcx.t) = FunctionSet.iter stack_colorer#run gcx.funcs
