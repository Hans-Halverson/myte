open Asm_instruction_definition

let b = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let ret = { InstructionDef.operands = [] }

let instr_def (instr : instr) : InstructionDef.t =
  match instr with
  | `B -> b
  | `Ret -> ret
  | _ -> failwith "Unknown X86_64 instr"
