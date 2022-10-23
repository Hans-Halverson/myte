open Asm_instruction_definition

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
  [{ OperandDef.use = Use; operand_type = RegMem }; { use = UseDef; operand_type = Register }]

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

let xor_pd = { InstructionDef.operands = operands_mr_usedef }

let ucomi_sd =
  {
    InstructionDef.operands =
      [{ use = Use; operand_type = Register }; { use = Use; operand_type = RegMem }];
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
  | `PushI -> push_i
  | `PushM -> push_m
  | `PopM -> pop_m
  | `MovIM _ -> mov_im
  | `MovMM _ -> mov_mm
  | `MovSX _ -> mov_zx
  | `MovZX _ -> mov_zx
  | `Lea _ -> lea
  | `NegM _ -> neg_m
  | `AddIM _ -> and_im
  | `AddMM _ -> and_mm
  | `SubIM _ -> sub_im
  | `SubMM _ -> sub_mm
  | `IMulMR _ -> imul_mr
  | `IMulIMR _ -> imul_imr
  | `IDiv _ -> idiv
  | `AddSD -> add_sd
  | `SubSD -> sub_sd
  | `MulSD -> mul_sd
  | `DivSD -> div_sd
  | `XorPD -> xor_pd
  | `UComiSD -> ucomi_sd
  | `NotM _ -> not_m
  | `AndIM _ -> and_im
  | `AndMM _ -> and_mm
  | `OrIM _ -> or_im
  | `OrMM _ -> or_mm
  | `XorIM _ -> xor_mm
  | `XorMM _ -> xor_mm
  | `ShlI _ -> shl_i
  | `ShlM _ -> shl_m
  | `ShrI _ -> shr_i
  | `ShrM _ -> shr_m
  | `SarI _ -> sar_i
  | `SarM _ -> sar_m
  | `CmpMI _ -> cmp_mi
  | `CmpMM _ -> cmp_mm
  | `TestMR _ -> test_mr
  | `SetCC _ -> set_cc
  | `ConvertDouble _ -> convert_double
  | `ConvertFloatToInt _ -> convert_float_to_int
  | `ConvertIntToFloat _ -> convert_int_to_float
  | `Jmp -> jmp
  | `JmpCC _ -> jmp_cc
  | `CallL _ -> call_l
  | `CallM _ -> call_m
  | `Ret -> ret
  | _ -> failwith "Unknown X86_64 instr"

(* Return the size of the i'th operand for this instruction. Size of immediate operands may not
   be accurate. *)
let instr_operand_size (instr : instr) (i : int) : X86_64.operand_size =
  match instr with
  (* Instructions where all operands have the same size *)
  | `MovIM size
  | `MovMM size
  | `Lea size
  | `NegM size
  | `NotM size
  | `AddIM size
  | `AddMM size
  | `SubIM size
  | `SubMM size
  | `IMulMR size
  | `IMulIMR size
  | `IDiv size
  | `AndIM size
  | `AndMM size
  | `OrIM size
  | `OrMM size
  | `XorIM size
  | `XorMM size
  | `ShlI size
  | `ShlM size
  | `ShrI size
  | `ShrM size
  | `SarI size
  | `SarM size
  | `CmpMI size
  | `CmpMM size
  | `TestMR size
  | `CallM (size, _)
  | `ConvertFloatToInt size
  | `ConvertIntToFloat size ->
    size
  (* Instructions where operands have different sizes *)
  | `MovSX (src_size, dest_size)
  | `MovZX (src_size, dest_size) ->
    if i == 0 then
      src_size
    else
      dest_size
  (* All stack operations are 64-bit *)
  | `PushI
  | `PushM
  | `PopM ->
    Size64
  (* Only 64-bit floating point operations are supported *)
  | `AddSD
  | `SubSD
  | `MulSD
  | `DivSD
  | `XorPD
  | `UComiSD ->
    Size64
  (* Instructions that only have byte operands *)
  | `SetCC _ -> Size8
  (* Instructions with no sized operands *)
  | `ConvertDouble _
  | `Jmp
  | `JmpCC _
  | `CallL _
  | `Ret ->
    failwith "No sized operands"
  | _ -> failwith "Unknown X86_64 instr"
