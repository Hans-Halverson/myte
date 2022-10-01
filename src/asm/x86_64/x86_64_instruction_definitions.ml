open X86_64_calling_conventions
open X86_64_register

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

(* Instruction opcode and metadata *)
type instr =
  (* Instruction Suffixes:
       R - virtual register
       I - immediate
       M - memory location or virtual register

       When multiple suffixes are used, first is source and second is dest when applicable.
       All MM instructions must contain at least one register as an argument.

       Unless otherwise noted, immediates can only be 8, 16, or 32 bits. *)
  (* Stack instructions, all implicitly have size of 64 bits *)
  | PushI
  | PushM
  | PopM
  (* Data instructions *)
  (* Allows 64-bit immediate. register_size is destination size which may not match immediate size *)
  | MovIM of register_size
  (* Allows 64-bit immediate. Allows SSE registers. register_size is destination size, or transferred size if SSE *)
  | MovMM of register_size
  (* Src size then dest size where src size < dest size *)
  | MovSX of register_size * register_size
  (* Src size then dest size where src size < dest size *)
  | MovZX of register_size * register_size
  (* Destination only supports 32 or 64 bit register *)
  | Lea of register_size
  (* Integer operations *)
  | NegM of register_size
  | AddIM of register_size
  | AddMM of register_size
  (* For sub instructions, right/dest := right/dest - left/src *)
  | SubIM of register_size
  | SubMM of register_size
  | IMulMR of register_size
  (* Only supports 16, 32, or 64-bit MR operands, and only supports 16 or 32-bit immediates *)
  | IMulIMR of register_size
  | IDiv of register_size
  (* Float operations, all have 64-bit size and require SSE registers *)
  | AddSD
  (* right/dist := (right/dest) - (left/src) *)
  | SubSD
  | MulSD
  (* right/dest := (right/dest) / (left/src) *)
  | DivSD
  | UComiSD
  (* Bitwise operations *)
  | NotM of register_size
  | AndIM of register_size
  | AndMM of register_size
  | OrIM of register_size
  | OrMM of register_size
  | XorIM of register_size
  (* Allows SSE registers (with 128 bit size), if SSE then destination must be a register *)
  | XorMM of register_size
  (* Bit shifts, all only support 8 bit immediates *)
  | ShlI of register_size
  | ShlM of register_size
  | ShrI of register_size
  | ShrM of register_size
  | SarI of register_size
  | SarM of register_size
  (* Comparisons *)
  | CmpMI of register_size
  | CmpMM of register_size
  | TestMR of register_size
  (* Only supports 8-bit destination *)
  | SetCC of condition_code
  (* Conversions *)
  | ConvertDouble of register_size (* Only supports 16, 32, and 64 byte sizes (cwd/cdq/cqo) *)
  (* Converts with truncation towards zero *)
  (* GP register size, must be 32 or 64-bit. Converts SSE mem to GP register. *)
  | ConvertFloatToInt of register_size
  (* GP register size, must be 32 or 64-bit. Converts GP mem to SSE register *)
  | ConvertIntToFloat of register_size
  (* Control flow *)
  | Jmp
  | JmpCC of condition_code
  | CallL of param_types
  | CallM of register_size * param_types
  | Ret

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

(* Operand def for base and index of a MemoryAddress *)
let address_reg_operand_def = { OperandDef.use = Use; operand_type = Register }

let operands_m_use = [{ OperandDef.use = Use; operand_type = RegMem }]

let operands_m_usedef = [{ OperandDef.use = UseDef; operand_type = RegMem }]

let operands_im_def =
  [{ OperandDef.use = Use; operand_type = Immediate }; { use = Def; operand_type = RegMem }]

let operands_im_usedef =
  [{ OperandDef.use = Use; operand_type = Immediate }; { use = UseDef; operand_type = RegMem }]

let operands_mm_def =
  [{ OperandDef.use = Use; operand_type = RegMem }; { use = Def; operand_type = RegMem }]

let operands_mm_usedef =
  [{ OperandDef.use = Use; operand_type = RegMem }; { use = UseDef; operand_type = RegMem }]

let operands_mr_def =
  [{ OperandDef.use = Use; operand_type = RegMem }; { use = Def; operand_type = Register }]

let operands_mr_usedef =
  [{ OperandDef.use = Use; operand_type = RegMem }; { use = Def; operand_type = Register }]

let push_i = { InstructionDef.operands = [{ use = Use; operand_type = Immediate }] }

let push_m = { InstructionDef.operands = operands_m_use }

let pop_m = { InstructionDef.operands = [{ use = Def; operand_type = RegMem }] }

let mov_im = { InstructionDef.operands = operands_im_def }

let mov_mm = { InstructionDef.operands = operands_mm_def }

let mov_sx = { InstructionDef.operands = operands_mr_def }

let mov_zx = { InstructionDef.operands = operands_mr_def }

let lea =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = MemoryAddress }; { use = Def; operand_type = Register }];
  }

let neg_m = { InstructionDef.operands = operands_m_usedef }

let add_im = { InstructionDef.operands = operands_im_usedef }

let add_mm = { InstructionDef.operands = operands_mm_usedef }

let sub_im = { InstructionDef.operands = operands_im_usedef }

let sub_mm = { InstructionDef.operands = operands_mm_usedef }

let imul_mr = { InstructionDef.operands = operands_mr_usedef }

let imul_imr =
  {
    InstructionDef.operands =
      [
        { use = Use; operand_type = Immediate };
        { use = Use; operand_type = RegMem };
        { use = Def; operand_type = Register };
      ];
  }

let idiv = { InstructionDef.operands = operands_m_use }

let add_sd = { InstructionDef.operands = operands_mr_usedef }

let sub_sd = { InstructionDef.operands = operands_mr_usedef }

let mul_sd = { InstructionDef.operands = operands_mr_usedef }

let div_sd = { InstructionDef.operands = operands_mr_usedef }

let ucomi_sd =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = RegMem }; { use = Use; operand_type = Register }];
  }

let not_m = { InstructionDef.operands = operands_m_usedef }

let and_im = { InstructionDef.operands = operands_im_usedef }

let and_mm = { InstructionDef.operands = operands_mm_usedef }

let or_im = { InstructionDef.operands = operands_im_usedef }

let or_mm = { InstructionDef.operands = operands_mm_usedef }

let xor_im = { InstructionDef.operands = operands_im_usedef }

let xor_mm = { InstructionDef.operands = operands_mm_usedef }

let shl_i = { InstructionDef.operands = operands_im_usedef }

let shl_m = { InstructionDef.operands = operands_m_usedef }

let shr_i = { InstructionDef.operands = operands_im_usedef }

let shr_m = { InstructionDef.operands = operands_m_usedef }

let sar_i = { InstructionDef.operands = operands_im_usedef }

let sar_m = { InstructionDef.operands = operands_m_usedef }

let cmp_mi =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = RegMem }; { use = Use; operand_type = Immediate }];
  }

let cmp_mm =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = RegMem }; { use = Use; operand_type = RegMem }];
  }

let test_mr =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = RegMem }; { use = Use; operand_type = Register }];
  }

let set_cc = { InstructionDef.operands = [{ use = Def; operand_type = RegMem }] }

let convert_double = { InstructionDef.operands = [] }

let convert_float_to_int = { InstructionDef.operands = operands_mr_def }

let convert_int_to_float = { InstructionDef.operands = operands_mr_def }

let jmp = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let jmp_cc = { InstructionDef.operands = [{ use = Use; operand_type = Block }] }

let call_l = { InstructionDef.operands = [{ use = Use; operand_type = Label }] }

let call_m = { InstructionDef.operands = operands_m_use }

let ret = { InstructionDef.operands = [] }

let instr_def (instr : instr) : InstructionDef.t =
  match instr with
  | PushI -> push_i
  | PushM -> push_m
  | PopM -> pop_m
  | MovIM _ -> mov_im
  | MovMM _ -> mov_mm
  | MovSX _ -> mov_zx
  | MovZX _ -> mov_zx
  | Lea _ -> lea
  | NegM _ -> neg_m
  | AddIM _ -> and_im
  | AddMM _ -> and_mm
  | SubIM _ -> sub_im
  | SubMM _ -> sub_mm
  | IMulMR _ -> imul_mr
  | IMulIMR _ -> imul_imr
  | IDiv _ -> idiv
  | AddSD -> add_sd
  | SubSD -> sub_sd
  | MulSD -> mul_sd
  | DivSD -> div_sd
  | UComiSD -> ucomi_sd
  | NotM _ -> not_m
  | AndIM _ -> and_im
  | AndMM _ -> and_mm
  | OrIM _ -> or_im
  | OrMM _ -> or_mm
  | XorIM _ -> xor_mm
  | XorMM _ -> xor_mm
  | ShlI _ -> shl_i
  | ShlM _ -> shl_m
  | ShrI _ -> shr_i
  | ShrM _ -> shr_m
  | SarI _ -> sar_i
  | SarM _ -> sar_m
  | CmpMI _ -> cmp_mi
  | CmpMM _ -> cmp_mm
  | TestMR _ -> test_mr
  | SetCC _ -> set_cc
  | ConvertDouble _ -> convert_double
  | ConvertFloatToInt _ -> convert_float_to_int
  | ConvertIntToFloat _ -> convert_int_to_float
  | Jmp -> jmp
  | JmpCC _ -> jmp_cc
  | CallL _ -> call_l
  | CallM _ -> call_m
  | Ret -> ret
