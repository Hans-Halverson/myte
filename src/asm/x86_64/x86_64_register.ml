open Asm_register
open Asm_instruction_definition

module Register = struct
  type t = Register.t

  type class_ =
    | GeneralClass
    | SSEClass

  let compare = Stdlib.compare
end

let all_sse_registers =
  RegSet.of_list
    [
      `XMM0;
      `XMM1;
      `XMM2;
      `XMM3;
      `XMM4;
      `XMM5;
      `XMM6;
      `XMM7;
      `XMM8;
      `XMM9;
      `XMM10;
      `XMM11;
      `XMM12;
      `XMM13;
      `XMM14;
      `XMM15;
    ]

let all_registers =
  RegSet.of_list [`A; `B; `C; `D; `SI; `DI; `SP; `BP; `R8; `R9; `R10; `R11; `R12; `R13; `R14; `R15]
  |> RegSet.union all_sse_registers

let get_reg_order (reg : Register.t) : int =
  match reg with
  | `A -> 0
  | `B -> 1
  | `C -> 2
  | `D -> 3
  | `SI -> 4
  | `DI -> 5
  | `SP -> 6
  | `BP -> 7
  | `R8 -> 8
  | `R9 -> 9
  | `R10 -> 10
  | `R11 -> 11
  | `R12 -> 12
  | `R13 -> 13
  | `R14 -> 14
  | `R15 -> 15
  (* SSE registers *)
  | `XMM0 -> 16
  | `XMM1 -> 17
  | `XMM2 -> 18
  | `XMM3 -> 19
  | `XMM4 -> 20
  | `XMM5 -> 21
  | `XMM6 -> 22
  | `XMM7 -> 23
  | `XMM8 -> 24
  | `XMM9 -> 25
  | `XMM10 -> 26
  | `XMM11 -> 27
  | `XMM12 -> 28
  | `XMM13 -> 29
  | `XMM14 -> 30
  | `XMM15 -> 31
  | _ -> failwith "Unknown X86_64 register"

let string_of_sized_reg (reg : Register.t) (size : X86_64.operand_size) : string =
  match (reg, size) with
  | (`A, Size64) -> "rax"
  | (`B, Size64) -> "rbx"
  | (`C, Size64) -> "rcx"
  | (`D, Size64) -> "rdx"
  | (`SI, Size64) -> "rsi"
  | (`DI, Size64) -> "rdi"
  | (`SP, Size64) -> "rsp"
  | (`BP, Size64) -> "rbp"
  | (`R8, Size64) -> "r8"
  | (`R9, Size64) -> "r9"
  | (`R10, Size64) -> "r10"
  | (`R11, Size64) -> "r11"
  | (`R12, Size64) -> "r12"
  | (`R13, Size64) -> "r13"
  | (`R14, Size64) -> "r14"
  | (`R15, Size64) -> "r15"
  | (`A, Size32) -> "eax"
  | (`B, Size32) -> "ebx"
  | (`C, Size32) -> "ecx"
  | (`D, Size32) -> "edx"
  | (`SI, Size32) -> "esi"
  | (`DI, Size32) -> "edi"
  | (`SP, Size32) -> "esp"
  | (`BP, Size32) -> "ebp"
  | (`R8, Size32) -> "r8d"
  | (`R9, Size32) -> "r9d"
  | (`R10, Size32) -> "r10d"
  | (`R11, Size32) -> "r11d"
  | (`R12, Size32) -> "r12d"
  | (`R13, Size32) -> "r13d"
  | (`R14, Size32) -> "r14d"
  | (`R15, Size32) -> "r15d"
  | (`A, Size16) -> "ax"
  | (`B, Size16) -> "bx"
  | (`C, Size16) -> "cx"
  | (`D, Size16) -> "dx"
  | (`SI, Size16) -> "si"
  | (`DI, Size16) -> "di"
  | (`SP, Size16) -> "sp"
  | (`BP, Size16) -> "bp"
  | (`R8, Size16) -> "r8w"
  | (`R9, Size16) -> "r9w"
  | (`R10, Size16) -> "r10w"
  | (`R11, Size16) -> "r11w"
  | (`R12, Size16) -> "r12w"
  | (`R13, Size16) -> "r13w"
  | (`R14, Size16) -> "r14w"
  | (`R15, Size16) -> "r15w"
  | (`A, Size8) -> "al"
  | (`B, Size8) -> "bl"
  | (`C, Size8) -> "cl"
  | (`D, Size8) -> "dl"
  | (`SI, Size8) -> "sil"
  | (`DI, Size8) -> "dil"
  | (`SP, Size8) -> "spl"
  | (`BP, Size8) -> "bpl"
  | (`R8, Size8) -> "r8b"
  | (`R9, Size8) -> "r9b"
  | (`R10, Size8) -> "r10b"
  | (`R11, Size8) -> "r11b"
  | (`R12, Size8) -> "r12b"
  | (`R13, Size8) -> "r13b"
  | (`R14, Size8) -> "r14b"
  | (`R15, Size8) -> "r15b"
  | (`XMM0, _) -> "xmm0"
  | (`XMM1, _) -> "xmm1"
  | (`XMM2, _) -> "xmm2"
  | (`XMM3, _) -> "xmm3"
  | (`XMM4, _) -> "xmm4"
  | (`XMM5, _) -> "xmm5"
  | (`XMM6, _) -> "xmm6"
  | (`XMM7, _) -> "xmm7"
  | (`XMM8, _) -> "xmm8"
  | (`XMM9, _) -> "xmm9"
  | (`XMM10, _) -> "xmm10"
  | (`XMM11, _) -> "xmm11"
  | (`XMM12, _) -> "xmm12"
  | (`XMM13, _) -> "xmm13"
  | (`XMM14, _) -> "xmm14"
  | (`XMM15, _) -> "xmm15"
  | _ -> failwith "Invalid X86_64 register and size"

let debug_string_of_reg (reg : Register.t) : string = string_of_sized_reg reg X86_64.Size64

let register_class reg =
  match reg with
  | `A
  | `B
  | `C
  | `D
  | `SI
  | `DI
  | `SP
  | `BP
  | `R8
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15 ->
    Register.GeneralClass
  | `XMM0
  | `XMM1
  | `XMM2
  | `XMM3
  | `XMM4
  | `XMM5
  | `XMM6
  | `XMM7
  | `XMM8
  | `XMM9
  | `XMM10
  | `XMM11
  | `XMM12
  | `XMM13
  | `XMM14
  | `XMM15 ->
    SSEClass
  | _ -> failwith "Unknown X86_64 register"
