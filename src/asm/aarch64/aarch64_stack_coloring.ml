open Asm
open Aarch64_builders
open Aarch64_gen_context

class stack_coloring =
  object
    inherit Asm_stack_coloring.stack_coloring

    method iter_operands instr f = instr_iter_all_operands instr f

    method calculate_live_out func =
      let liveness_analyzer = new Aarch64_liveness_analysis.vslots_liveness_analyzer func in
      let (_, live_out) = liveness_analyzer#analyze () in
      live_out
  end

let stack_colorer = new stack_coloring

let color_stack_slots ~(gcx : Gcx.t) = FunctionSet.iter stack_colorer#run gcx.funcs
