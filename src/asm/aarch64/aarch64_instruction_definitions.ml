open Asm_instruction_definition

let operands_rr_def =
  [{ OperandDef.use = Def; operand_type = Register }; { use = Use; operand_type = Register }]

let mov_i =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = Use; operand_type = Immediate };
        { use = Use; operand_type = Immediate };
      ];
  }

let mov_r = { InstructionDef.operands = operands_rr_def }

let b = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let bl = { InstructionDef.operands = [{ use = Use; operand_type = Function }] }

let blr = { InstructionDef.operands = [{ use = Use; operand_type = Register }] }

let ret = { InstructionDef.operands = [] }

let instr_def (instr : instr) : InstructionDef.t =
  match instr with
  | `MovI _ -> mov_i
  | `MovR _ -> mov_r
  | `B -> b
  | `BL _ -> bl
  | `BLR _ -> blr
  | `Ret -> ret
  | _ -> failwith "Unknown X86_64 instr"

(* Return the register size for this instruction *)
let instr_register_size (instr : instr) : AArch64.register_size =
  match instr with
  (* Instructions where all registers have the same size *)
  | `MovI (size, _)
  | `MovR size ->
    size
  (* Registers must be 64 bits *)
  | `BLR _
  (* Instructions with no sized operands *)
  | `B
  | `BL _
  | `Ret ->
    failwith "No sized operands"
  | _ -> failwith "Unknown X86_64 instr"
