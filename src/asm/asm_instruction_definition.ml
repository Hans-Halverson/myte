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
    | `CallL of param_types
    | `CallM of operand_size * param_types
    | `Ret
    ]
end

module AArch64 = struct
  type register_size =
    | Size32
    | Size64

  type instr =
    (* Instruction Suffixes:
         R - register
         I - immediate

         Leftmost operands are dest, rightmost operands are sources.

         In description, R registers can be an X or W depending on the register_size, unless otherwise noted. *)
    [ (* Mov Rd, #imm16 *)
      `MovI of register_size
    | (* Mov Rd, Rs *)
      `MovR of
      register_size
      (* MovK Rd, #imm16, LSL #shift where #shift is one of 0, 16, 32, 64 *)
    | `MovK of register_size
    | (* Unconditional branch to label *)
      `B
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
    | Immediate
    | MemoryAddress
    | Register
    | RegMem
    | Block
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
