open Asm_instruction_definition

let operands_rr =
  [{ OperandDef.use = Def; operand_type = Register }; { use = Use; operand_type = Register }]

let operands_rrr =
  [
    { OperandDef.use = Def; operand_type = Register };
    { use = Use; operand_type = Register };
    { use = Use; operand_type = Register };
  ]

let operands_rrrr =
  [
    { OperandDef.use = Def; operand_type = Register };
    { use = Use; operand_type = Register };
    { use = Use; operand_type = Register };
    { use = Use; operand_type = Register };
  ]

let operands_add_sub_i =
  [
    { OperandDef.use = Def; operand_type = Register };
    { use = Use; operand_type = Register };
    { use = Use; operand_type = Immediate };
    { use = Use; operand_type = Immediate };
  ]

let mov_i =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = Use; operand_type = Immediate };
        { use = Use; operand_type = Immediate };
      ];
  }

let mov_r = { InstructionDef.operands = operands_rr }

let add_i = { InstructionDef.operands = operands_add_sub_i }

let add_r = { InstructionDef.operands = operands_rrr }

let sub_i = { InstructionDef.operands = operands_add_sub_i }

let sub_r = { InstructionDef.operands = operands_rrr }

let mul = { InstructionDef.operands = operands_rrr }

let sdiv = { InstructionDef.operands = operands_rrr }

let msub = { InstructionDef.operands = operands_rrrr }

let sxt = { InstructionDef.operands = operands_rr }

let b = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let bl = { InstructionDef.operands = [{ use = Use; operand_type = Function }] }

let blr = { InstructionDef.operands = [{ use = Use; operand_type = Register }] }

let ret = { InstructionDef.operands = [] }

let instr_def (instr : instr) : InstructionDef.t =
  match instr with
  | `MovI _ -> mov_i
  | `MovR _ -> mov_r
  | `AddI _ -> add_i
  | `AddR _ -> add_r
  | `SubI _ -> sub_i
  | `SubR _ -> sub_r
  | `Mul _ -> mul
  | `SDiv _ -> sdiv
  | `MSub _ -> msub
  | `Sxt _ -> sxt
  | `B -> b
  | `BL _ -> bl
  | `BLR _ -> blr
  | `Ret -> ret
  | _ -> failwith "Unknown X86_64 instr"

(* Return the register size of the i'th operand for this instruction. Only guaranteed to be
   accurate for register operands. *)
let instr_register_size (instr : instr) (i : int) : AArch64.register_size =
  match instr with
  (* Instructions where all registers have the same size *)
  | `MovI (size, _)
  | `MovR size
  | `AddI size
  | `AddR size
  | `SubI size
  | `SubR size
  | `Mul size
  | `SDiv size
  | `MSub size ->
    size
  (* Registers must be 64 bits *)
  | `BLR _ -> Size64
  (* Instructions with operands that have different register sizes *)
  | `Sxt (size, _) ->
    if i == 0 then
      size
    else
      Size32
  (* Instructions with no sized operands *)
  | `B
  | `BL _
  | `Ret ->
    failwith "No sized operands"
  | _ -> failwith "Unknown X86_64 instr"
