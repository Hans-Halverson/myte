open Aarch64_instruction_definitions
open Asm
open Asm_instruction_definition

(*
 * ============================
 *            Uses
 * ============================
 *)

let instr_iter_all_operands (instr : Instruction.t) (f : Operand.t -> OperandDef.t -> unit) : unit =
  let instr_def = instr_def instr.instr in
  List.iteri
    (fun i (operand_def : OperandDef.t) ->
      let operand = instr.operands.(i) in
      f operand operand_def)
    instr_def.operands
