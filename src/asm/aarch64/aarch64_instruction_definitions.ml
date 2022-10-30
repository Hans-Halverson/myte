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

let operands_bitwise_i =
  [
    { OperandDef.use = Def; operand_type = Register };
    { use = Use; operand_type = Register };
    { use = Use; operand_type = Immediate };
  ]

let operands_shift_i =
  [
    { OperandDef.use = Def; operand_type = Register };
    { use = Use; operand_type = Register };
    { use = Use; operand_type = Immediate };
  ]

let operands_bfx =
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

let neg = { InstructionDef.operands = operands_rr }

let mvn = { InstructionDef.operands = operands_rr }

let and_i = { InstructionDef.operands = operands_bitwise_i }

let lsl_i = { InstructionDef.operands = operands_shift_i }

let lsl_r = { InstructionDef.operands = operands_rrr }

let lsr_i = { InstructionDef.operands = operands_shift_i }

let lsr_r = { InstructionDef.operands = operands_rrr }

let asr_i = { InstructionDef.operands = operands_shift_i }

let asr_r = { InstructionDef.operands = operands_rrr }

let sbfx = { InstructionDef.operands = operands_bfx }

let ubfx = { InstructionDef.operands = operands_bfx }

let cmp_i =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = Register }; { use = Use; operand_type = Immediate }];
  }

let cmp_r =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = Register }; { use = Use; operand_type = Register }];
  }

let cmn_i =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = Register }; { use = Use; operand_type = Immediate }];
  }

let cset = { InstructionDef.operands = [{ use = Def; operand_type = Register }] }

let sxt = { InstructionDef.operands = operands_rr }

let b = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let b_cond = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let cbz =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = Register }; { use = Use; operand_type = Block }];
  }

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
  | `Neg _ -> neg
  | `Mvn _ -> mvn
  | `AndI _ -> and_i
  | `LslI _ -> lsl_i
  | `LslR _ -> lsl_r
  | `LsrI _ -> lsr_i
  | `LsrR _ -> lsr_r
  | `AsrI _ -> asr_i
  | `AsrR _ -> asr_r
  | `Sbfx _ -> sbfx
  | `Ubfx _ -> ubfx
  | `CmpI _ -> cmp_i
  | `CmpR _ -> cmp_r
  | `CmnI _ -> cmn_i
  | `CSet _ -> cset
  | `Sxt _ -> sxt
  | `B -> b
  | `BCond _ -> b_cond
  | `Cbz _
  | `BL _ ->
    bl
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
  | `MSub size
  | `Neg size
  | `Mvn size
  | `AndI size
  | `LslI size
  | `LslR size
  | `LsrI size
  | `LsrR size
  | `AsrI size
  | `AsrR size
  | `Sbfx size
  | `Ubfx size
  | `CmpI size
  | `CmpR (size, _)
  | `CmnI size
  | `CSet (size, _)
  | `Cbz size ->
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
  | `BCond _
  | `BL _
  | `Ret ->
    failwith "No sized operands"
  | _ -> failwith "Unknown X86_64 instr"
