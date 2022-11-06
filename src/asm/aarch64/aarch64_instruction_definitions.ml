open Aarch64_asm
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

let adr_p =
  {
    InstructionDef.operands =
      [{ OperandDef.use = Def; operand_type = Register }; { use = Use; operand_type = Label }];
  }

let add_i = { InstructionDef.operands = operands_add_sub_i }

let add_r =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let sub_i = { InstructionDef.operands = operands_add_sub_i }

let sub_r = { InstructionDef.operands = operands_rrr }

let mul = { InstructionDef.operands = operands_rrr }

let sdiv = { InstructionDef.operands = operands_rrr }

let msub = { InstructionDef.operands = operands_rrrr }

let neg = { InstructionDef.operands = operands_rr }

let mvn = { InstructionDef.operands = operands_rr }

let and_i = { InstructionDef.operands = operands_bitwise_i }

let and_r = { InstructionDef.operands = operands_rrr }

let orr_i = { InstructionDef.operands = operands_bitwise_i }

let orr_r = { InstructionDef.operands = operands_rrr }

let eor_i = { InstructionDef.operands = operands_bitwise_i }

let eor_r = { InstructionDef.operands = operands_rrr }

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

let ldr_i_offset =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let ldr_i_pre_post =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = UseDef; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let str_i_offset =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Use; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let str_i_pre_post =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Use; operand_type = Register };
        { use = UseDef; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let ldr_r =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { OperandDef.use = Use; operand_type = Register };
        { OperandDef.use = Use; operand_type = Register };
        { OperandDef.use = Use; operand_type = Immediate };
      ];
  }

let str_r =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Use; operand_type = Register };
        { OperandDef.use = Use; operand_type = Register };
        { OperandDef.use = Use; operand_type = Register };
        { OperandDef.use = Use; operand_type = Immediate };
      ];
  }

let ldp_offset =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = Def; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let ldp_pre_post =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Def; operand_type = Register };
        { use = Def; operand_type = Register };
        { use = UseDef; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let stp_offset =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Use; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let stp_pre_post =
  {
    InstructionDef.operands =
      [
        { OperandDef.use = Use; operand_type = Register };
        { use = Use; operand_type = Register };
        { use = UseDef; operand_type = Register };
        { use = Use; operand_type = Immediate };
      ];
  }

let fmov_i =
  {
    InstructionDef.operands =
      [{ OperandDef.use = Def; operand_type = Register }; { use = Use; operand_type = Immediate }];
  }

let fmov_r =
  {
    InstructionDef.operands =
      [{ OperandDef.use = Def; operand_type = Register }; { use = Use; operand_type = Register }];
  }

let fadd = { InstructionDef.operands = operands_rrr }

let fsub = { InstructionDef.operands = operands_rrr }

let fmul = { InstructionDef.operands = operands_rrr }

let fdiv = { InstructionDef.operands = operands_rrr }

let fneg = { InstructionDef.operands = operands_rr }

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
  | `AdrP -> adr_p
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
  | `AndR _ -> and_r
  | `OrrI _ -> orr_i
  | `OrrR _ -> orr_r
  | `EorI _ -> eor_i
  | `EorR _ -> eor_r
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
  | `LdrI (_, _, _, Offset) -> ldr_i_offset
  | `LdrI (_, _, _, (PreIndex | PostIndex)) -> ldr_i_pre_post
  | `StrI (_, Offset) -> str_i_offset
  | `StrI (_, (PreIndex | PostIndex)) -> str_i_pre_post
  | `LdrR _ -> ldr_r
  | `StrR _ -> str_r
  | `Ldp (_, Offset) -> ldp_offset
  | `Ldp (_, (PreIndex | PostIndex)) -> ldp_pre_post
  | `Stp (_, Offset) -> stp_offset
  | `Stp (_, (PreIndex | PostIndex)) -> stp_pre_post
  | `FMovI -> fmov_i
  | `FMovR -> fmov_r
  | `FAdd -> fadd
  | `FSub -> fsub
  | `FMul -> fmul
  | `FDiv -> fdiv
  | `FNeg -> fneg
  | `B -> b
  | `BCond _ -> b_cond
  | `Cbz _ -> cbz
  | `BL _ -> bl
  | `BLR _ -> blr
  | `Ret -> ret
  | _ -> failwith "Unknown aarch64 instr"

(* Return the register size of the i'th operand for this instruction. Only guaranteed to be
   accurate for register operands. *)
let instr_register_size (instr : instr) (i : int) : AArch64.register_size =
  let size_of_address_extend (extend : AArch64.addressing_extend) : AArch64.register_size =
    match extend with
    | LSL -> Size64
    | UXTW
    | SXTW ->
      Size32
  in
  match instr with
  (* Instructions where all registers have the same size *)
  | `MovI (size, _)
  | `MovR size
  | `AddI size
  | `AddR (size, _)
  | `SubI size
  | `SubR size
  | `Mul size
  | `SDiv size
  | `MSub size
  | `Neg size
  | `Mvn size
  | `AndI size
  | `AndR size
  | `OrrI size
  | `OrrR size
  | `EorI size
  | `EorR size
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
  | `AdrP
  | `BLR _
  | `FMovI
  | `FMovR
  | `FAdd
  | `FSub
  | `FMul
  | `FDiv
  | `FNeg ->
    Size64
  (* Instructions with operands that have different register sizes *)
  | `Sxt (size, _) ->
    if i == 0 then
      size
    else
      Size32
  | `LdrI (size, _, _, _) ->
    if i == 0 then
      size
    else
      Size64
  | `StrI (size, _) ->
    if i == 0 then
      register_size_of_subregister_size size
    else
      Size64
  | `LdrR (size, _, _, extend) ->
    if i == 0 then
      size
    else if i == 2 then
      size_of_address_extend extend
    else
      Size64
  | `StrR (size, extend) ->
    if i == 0 then
      register_size_of_subregister_size size
    else if i == 2 then
      size_of_address_extend extend
    else
      Size64
  | `Ldp (size, _)
  | `Stp (size, _) ->
    if i == 0 || i == 1 then
      size
    else
      Size64
  (* Instructions with no sized operands *)
  | `B
  | `BCond _
  | `BL _
  | `Ret ->
    failwith "No sized operands"
  | _ -> failwith "Unknown X86_64 instr"
