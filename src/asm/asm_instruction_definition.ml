open Asm_calling_convention

module X86_64 = struct
  type operand_size =
    | Size8
    | Size16
    | Size32
    | Size64
    | Size128

  type condition_code =
    | E
    | NE
    | L
    | LE
    | G
    | GE
    | B
    | BE
    | A
    | AE
    | P
    | NP

  type instr =
    (* Instruction Suffixes:
         R - virtual register
         I - immediate
         M - memory location or virtual register

         When multiple suffixes are used, first is source and second is dest when applicable.
         All MM instructions must contain at least one register as an argument.

         Unless otherwise noted, immediates can only be 8, 16, or 32 bits. *)
    [ (* Stack instructions, all implicitly have size of 64 bits *)
      `PushI
    | `PushM
    | `PopM
    | (* Data instructions *)
      (* Allows 64-bit immediate. operand_size is destination size which may not match immediate size *)
      `MovIM of
      operand_size
    | (* Allows 64-bit immediate. Allows SSE registers. operand_size is destination size, or transferred size if SSE *)
      `MovMM of
      operand_size
    | (* Src size then dest size where src size < dest size *)
      `MovSX of operand_size * operand_size
    | (* Src size then dest size where src size < dest size *)
      `MovZX of operand_size * operand_size
    | (* Destination only supports 32 or 64 bit register *)
      `Lea of operand_size
    | (* Integer operations *)
      `NegM of operand_size
    | `AddIM of operand_size
    | `AddMM of operand_size
    | (* For sub instructions, right/dest := right/dest - left/src *)
      `SubIM of operand_size
    | `SubMM of operand_size
    | `IMulMR of operand_size
    | (* Only supports 16, 32, or 64-bit MR operands, and only supports 16 or 32-bit immediates *)
      `IMulIMR of
      operand_size
    | `IDiv of operand_size
    | (* Float operations, all have 64-bit size and require SSE registers *)
      `AddSD
    | (* right/dist := (right/dest) - (left/src) *)
      `SubSD
    | `MulSD
    | (* right/dest := (right/dest) / (left/src) *)
      `DivSD
    | `XorPD
    | `UComiSD
    | (* Bitwise operations *)
      `NotM of operand_size
    | `AndIM of operand_size
    | `AndMM of operand_size
    | `OrIM of operand_size
    | `OrMM of operand_size
    | `XorIM of operand_size
    | `XorMM of operand_size
    | (* Bit shifts, all only support 8 bit immediates *)
      `ShlI of operand_size
    | `ShlM of operand_size
    | `ShrI of operand_size
    | `ShrM of operand_size
    | `SarI of operand_size
    | `SarM of operand_size
    | (* Comparisons *)
      `CmpMI of operand_size
    | `CmpMM of operand_size
    | `TestMR of operand_size
    | (* Only supports 8-bit destination *)
      `SetCC of condition_code
    | (* Conversions *)
      `ConvertDouble of
      operand_size (* Only supports 16, 32, and 64 byte sizes (cwd/cdq/cqo) *)
    | (* Converts with truncation towards zero *)
      (* GP register size, must be 32 or 64-bit. Converts SSE mem to GP register. *)
      `ConvertFloatToInt of
      operand_size
    | (* GP register size, must be 32 or 64-bit. Converts GP mem to SSE register *)
      `ConvertIntToFloat of
      operand_size
    | (* Control flow *)
      `Jmp
    | `JmpCC of condition_code
    | `CallL of param_types * calling_convention
    | `CallM of operand_size * param_types * calling_convention
    | `Ret
    ]
end

module AArch64 = struct
  type register_size =
    | Size32
    | Size64

  type subregister_size =
    | B
    | H
    | W
    | X

  type movi_suffix =
    | Z
    | N
    | K

  type cond =
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    (* Minus, negative (N == 1) *)
    | MI
    (* Lower or same !(C == 1 && Z == 0) *)
    | LS

  type extend =
    | UXTB
    | UXTH
    | UXTW
    | UXTX
    | SXTB
    | SXTH
    | SXTW
    | SXTX

  type addressing_mode =
    (* Do not update base register *)
    | Offset
    (* Update base register before indexing *)
    | PreIndex
    (* Update base register after indexing *)
    | PostIndex

  type addressing_extend =
    | UXTW
    | SXTW
    | LSL

  type instr =
    (* Instruction Suffixes:
         R - register
         I - immediate

         Leftmost operands are dest, rightmost operands are sources.

         In description, R registers can be an X or W depending on the register_size, unless otherwise noted. *)
    [ (* Mov Rd, #imm16, LSL #shift where #shift is one of 0, 16, 32, 64.
         Suffixes: Z for zeroing, N is for writing inverse, K is for keeping other bits *)
      `MovI of
      register_size * movi_suffix
    | (* Mov Rd, Rs *)
      `MovR of register_size
    | (* LdrI/StrI Rd, [Rs, #imm] with any addressing mode.
         - (all modes) #imm must be between -256 and 255
         - (B) #imm must be between 0 and 4096
         - (H) #imm must be a multiple of 2 between 0 and 8190
         - (W) #imm must be a multiple of 4 between 0 and 16380
         - (X) #imm must be a multiple of 8 between 0 and 32760

         For signed loads the dest reg may be any register larger than the loaded size.
         For unsigned loads the dest reg must be the smallest register that fits the loaded size.
         For stores the source reg must be the smallest register that fits the stored size. *)
      `LdrI of
      (* Dest reg size *)
      register_size
      * (* Size to load *) subregister_size
      * (* Is signed *) bool
      * (* Addressing mode *) addressing_mode
    | `StrI of (* Stored size *) subregister_size * (* Addressing mode *) addressing_mode
    | (* LdrR/StrR may have the following forms depending on number of operands supplied:
         - Rd, [Rs]
         - Rd, [Rs1, Rs2]
         - Rd, [Rs1, Rs2, EXTEND #imm ] where #imm is a left shift of Rs2 and must be one of
           0, 1, 2, 3. #imm is optional if EXTEND is UXTW or SXTW, but is required for LSL. *)
      `LdrR of
      (* Dest reg size *)
      register_size
      * (* Size to load *) subregister_size
      * (* Is signed *) bool
      * (* Extend of offset register *) addressing_extend
    | `StrR of (* Stored size *)
      subregister_size * (* Extend of offset register *) addressing_extend
    | (* Ldp/Stp Rd1, Rd2, [Rs, #imm] with any addressing mode. For 32-bit registers #imm is any
         multiple of 4 between -256 and 252, for 64-bit registers #imm is any multiple of 8
         between -512 and 504. *)
      `Ldp of
      register_size * addressing_mode
    | `Stp of register_size * addressing_mode
    | (* AdrP Rd, label loads the 4kb page address of the label to a register. The lower 12 bits
         of the label's address must be added later to form the complete address. *)
      `AdrP
    | (* Add Rd, Rs, #imm12, LSL #shift where #shift is one of 0, 12 *)
      `AddI of register_size
    | (* AddR may have the following forms depending on number of operands suplied:
         - Add Rd, Rs1, Rs2
         - Add Rd, Rs1, Rs2, EXTEND #imm where #imm is 0, 1, 2, 3, 4. Cannot use extend if
           Rd or Rs1 are SP. *)
      `AddR of
      register_size * extend
    | (* Add Rd, Rs, #imm12, LSL #shift where #shift is one of 0, 12 *)
      `SubI of register_size
    | (* Add Rd, Rs1, Rs2 *)
      `SubR of register_size
    | (* Mul Rd, Rs1, Rs2 *)
      `Mul of register_size
    | (* SDiv Rd, Rs1, Rs2 *)
      `SDiv of register_size
    | (* MSub Rd, Rs1, Rs2, Rs3 where Rd = Rs3 - Rs1 * Rs2 *)
      `MSub of register_size
    | (* Neg Rd, Rs *)
      `Neg of register_size
    | (* Bitwise not: Mvn Rd, Rs *)
      `Mvn of register_size
    | (* Immediate bitwise ops: Op Rd, Rs, #imm where #imm is a bitmask immediate
         Register bitwise ops: Op Rd, Rs1, Rs2 *)
      `AndI of
      register_size
    | `AndR of register_size
    | `OrrI of register_size
    | `OrrR of register_size
    | `EorI of register_size
    | `EorR of register_size
    | (* Immediate shifts: Shift Rd, Rs, #shift where 0 <= shift < 32 or 64
         Register shifts: Shift Rd, Rs1, Rs2 where only low 5 (or 6) bits of Rs2 are used *)
      `LslI of
      register_size
    | `LslR of register_size
    | `LsrI of register_size
    | `LsrR of register_size
    | `AsrI of register_size
    | `AsrR of register_size
    | (* Signed and unsigned bitfield extract: Bfx Rd, Rs, #imm1, #imm2.
         #imm1 is the number of bits to rotate right, and #imm2 is the number of bits on the right
         to extract. The rest of the bits will be sign extended or not depending on instruction. *)
      `Sbfx of
      register_size
    | `Ubfx of register_size
    | (* Cmp Rs, #imm12 *)
      `CmpI of register_size
    | (* Cmp Rs1, Rs2 where Rs2 has the specified extension *)
      `CmpR of register_size * extend
    | (* Cmn Rs, #imm12 *)
      `CmnI of register_size
    | (* CSet Rs with the specified cond *)
      `CSet of register_size * cond
    | (* Sign extend subregister (Byte, Halfword, or Word) to full register (W or X) *)
      `Sxt of
      register_size * subregister_size
    | (* FMov Dd, #imm where imm has a sign, 3 bit exponent, and 4 bit mantissa *)
      `FMovI
    | (* FMov Dd, Ds or FMov Dd, XZR *)
      `FMovR
    | (* FAdd Dd, Ds1, Ds2 *)
      `FAdd
    | (* FSub Dd, Ds1, Ds2 *)
      `FSub
    | (* FMul Dd, Ds1, Ds2 *)
      `FMul
    | (* FDiv Dd, Ds1, Ds2 *)
      `FDiv
    | (* FNeg Dd, Ds *)
      `FNeg
    | (* FCmpZ Rs, #0.0 *)
      `FCmpZ
    | (* FCmpR Rs1, Rs2 *)
      `FCmpR
    | (* Unconditional branch to label *)
      `B
    | (* Conditional branch to label *)
      `BCond of cond
    | (* Branch to label if Rs is zero: Cbz Rs, label *)
      `Cbz of register_size
    | (* Function call to label *)
      `BL of param_types * calling_convention
    | (* Indirect function call through register. Register is always 64 bits. *)
      `BLR of
      param_types * calling_convention
    | `Ret
    ]
end

type instr =
  [ AArch64.instr
  | X86_64.instr
  ]

module rec InstructionDef : sig
  type t = { operands: OperandDef.t list }
end =
  InstructionDef

and OperandDef : sig
  type t = {
    use: use;
    operand_type: operand_type;
  }

  and use =
    | Use
    | Def
    | UseDef

  and operand_type =
    | Register
    | Immediate
    | MemoryAddress
    | RegMem
    | Block
    | Function
    | Label
end =
  OperandDef

let operand_is_use (operand_def : OperandDef.t) =
  match operand_def.use with
  | Use
  | UseDef ->
    true
  | _ -> false

let operand_is_def (operand_def : OperandDef.t) =
  match operand_def.use with
  | Def
  | UseDef ->
    true
  | _ -> false
