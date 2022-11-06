open Asm
open Aarch64_gen_context

class stack_coloring =
  object
    inherit Asm_stack_coloring.stack_coloring

    method calculate_live_out func =
      let liveness_analyzer = new Aarch64_liveness_analysis.vslots_liveness_analyzer func in
      let (_, live_out) = liveness_analyzer#analyze () in
      live_out

    method find_vslot_use_defs instr =
      (* After register allocation vslots will only apper as the second argument of spill instructions *)
      match instr.instr with
      | `SpillUse _ ->
        let operand = instr.operands.(1) in
        if operand.value == VirtualStackSlot then
          (OperandSet.singleton operand, OperandSet.empty)
        else
          (OperandSet.empty, OperandSet.empty)
      | `SpillDef _ ->
        let operand = instr.operands.(1) in
        if operand.value == VirtualStackSlot then
          (OperandSet.empty, OperandSet.singleton operand)
        else
          (OperandSet.empty, OperandSet.empty)
      | _ -> (OperandSet.empty, OperandSet.empty)
  end

let stack_colorer = new stack_coloring

let color_stack_slots ~(gcx : Gcx.t) = FunctionSet.iter stack_colorer#run gcx.funcs
