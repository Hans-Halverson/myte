open Asm_instruction_definition
open Asm_register

module Register = struct
  type t = Register.t

  type class_ =
    | GeneralClass
    | VectorClass
end

let general_purpose_registers =
  RegSet.of_list
    [
      `R0;
      `R1;
      `R2;
      `R3;
      `R4;
      `R5;
      `R6;
      `R7;
      `R8;
      `R9;
      `R10;
      `R11;
      `R12;
      `R13;
      `R14;
      `R15;
      `R16;
      `R17;
      `R18;
      `R19;
      `R20;
      `R21;
      `R22;
      `R23;
      `R24;
      `R25;
      `R26;
      `R27;
      `R28;
      `R29;
      `R30;
      `R31;
    ]

let vector_registers =
  RegSet.of_list
    [
      `V0;
      `V1;
      `V2;
      `V3;
      `V4;
      `V5;
      `V6;
      `V7;
      `V8;
      `V9;
      `V10;
      `V11;
      `V12;
      `V13;
      `V14;
      `V15;
      `V16;
      `V17;
      `V18;
      `V19;
      `V20;
      `V21;
      `V22;
      `V23;
      `V24;
      `V25;
      `V26;
      `V27;
      `V28;
      `V29;
      `V30;
      `V31;
    ]

(* R30 is the link register *)
let lr : Register.t = `R30

(* R31 is SP or ZR depending on context *)
let sp : Register.t = `R31
let zr : Register.t = `R31

let all_registers = RegSet.union general_purpose_registers vector_registers

let string_of_sized_reg (reg : Register.t) (size : AArch64.register_size) : string =
  let general_prefix () =
    match size with
    | Size32 -> "w"
    | Size64 -> "x"
  in
  let vector_prefix () =
    match size with
    | Size64 -> "d"
    | _ -> failwith "Invalid AArch64 register and size"
  in
  match reg with
  | `R0 -> general_prefix () ^ "0"
  | `R1 -> general_prefix () ^ "1"
  | `R2 -> general_prefix () ^ "2"
  | `R3 -> general_prefix () ^ "3"
  | `R4 -> general_prefix () ^ "4"
  | `R5 -> general_prefix () ^ "5"
  | `R6 -> general_prefix () ^ "6"
  | `R7 -> general_prefix () ^ "7"
  | `R8 -> general_prefix () ^ "8"
  | `R9 -> general_prefix () ^ "9"
  | `R10 -> general_prefix () ^ "10"
  | `R11 -> general_prefix () ^ "11"
  | `R12 -> general_prefix () ^ "12"
  | `R13 -> general_prefix () ^ "13"
  | `R14 -> general_prefix () ^ "14"
  | `R15 -> general_prefix () ^ "15"
  | `R16 -> general_prefix () ^ "16"
  | `R17 -> general_prefix () ^ "17"
  | `R18 -> general_prefix () ^ "18"
  | `R19 -> general_prefix () ^ "19"
  | `R20 -> general_prefix () ^ "20"
  | `R21 -> general_prefix () ^ "21"
  | `R22 -> general_prefix () ^ "22"
  | `R23 -> general_prefix () ^ "23"
  | `R24 -> general_prefix () ^ "24"
  | `R25 -> general_prefix () ^ "25"
  | `R26 -> general_prefix () ^ "26"
  | `R27 -> general_prefix () ^ "27"
  | `R28 -> general_prefix () ^ "28"
  | `R29 -> general_prefix () ^ "29"
  | `R30 -> general_prefix () ^ "30"
  | `R31 -> general_prefix () ^ "31"
  | `V0 -> vector_prefix () ^ "0"
  | `V1 -> vector_prefix () ^ "1"
  | `V2 -> vector_prefix () ^ "2"
  | `V3 -> vector_prefix () ^ "3"
  | `V4 -> vector_prefix () ^ "4"
  | `V5 -> vector_prefix () ^ "5"
  | `V6 -> vector_prefix () ^ "6"
  | `V7 -> vector_prefix () ^ "7"
  | `V8 -> vector_prefix () ^ "8"
  | `V9 -> vector_prefix () ^ "9"
  | `V10 -> vector_prefix () ^ "10"
  | `V11 -> vector_prefix () ^ "11"
  | `V12 -> vector_prefix () ^ "12"
  | `V13 -> vector_prefix () ^ "13"
  | `V14 -> vector_prefix () ^ "14"
  | `V15 -> vector_prefix () ^ "15"
  | `V16 -> vector_prefix () ^ "16"
  | `V17 -> vector_prefix () ^ "17"
  | `V18 -> vector_prefix () ^ "18"
  | `V19 -> vector_prefix () ^ "19"
  | `V20 -> vector_prefix () ^ "20"
  | `V21 -> vector_prefix () ^ "21"
  | `V22 -> vector_prefix () ^ "22"
  | `V23 -> vector_prefix () ^ "23"
  | `V24 -> vector_prefix () ^ "24"
  | `V25 -> vector_prefix () ^ "25"
  | `V26 -> vector_prefix () ^ "26"
  | `V27 -> vector_prefix () ^ "27"
  | `V28 -> vector_prefix () ^ "28"
  | `V29 -> vector_prefix () ^ "29"
  | `V30 -> vector_prefix () ^ "30"
  | `V31 -> vector_prefix () ^ "31"
  | _ -> failwith "Invalid AArch64 register"

let debug_string_of_reg (reg : Register.t) : string = string_of_sized_reg reg AArch64.Size64

let register_class reg =
  match reg with
  | `R0
  | `R1
  | `R2
  | `R3
  | `R4
  | `R5
  | `R6
  | `R7
  | `R8
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15
  | `R16
  | `R17
  | `R18
  | `R19
  | `R20
  | `R21
  | `R22
  | `R23
  | `R24
  | `R25
  | `R26
  | `R27
  | `R28
  | `R29
  | `R30
  | `R31 ->
    Register.GeneralClass
  | `V0
  | `V1
  | `V2
  | `V3
  | `V4
  | `V5
  | `V6
  | `V7
  | `V8
  | `V9
  | `V10
  | `V11
  | `V12
  | `V13
  | `V14
  | `V15
  | `V16
  | `V17
  | `V18
  | `V19
  | `V20
  | `V21
  | `V22
  | `V23
  | `V24
  | `V25
  | `V26
  | `V27
  | `V28
  | `V29
  | `V30
  | `V31 ->
    VectorClass
  | _ -> failwith "Unknown AArch64 register"

let get_reg_order (reg : Register.t) : int =
  match reg with
  | `R0 -> 0
  | `R1 -> 1
  | `R2 -> 2
  | `R3 -> 3
  | `R4 -> 4
  | `R5 -> 5
  | `R6 -> 6
  | `R7 -> 7
  | `R8 -> 8
  | `R9 -> 9
  | `R10 -> 10
  | `R11 -> 11
  | `R12 -> 12
  | `R13 -> 13
  | `R14 -> 14
  | `R15 -> 15
  | `R16 -> 16
  | `R17 -> 17
  | `R18 -> 18
  | `R19 -> 19
  | `R20 -> 20
  | `R21 -> 21
  | `R22 -> 22
  | `R23 -> 23
  | `R24 -> 24
  | `R25 -> 25
  | `R26 -> 26
  | `R27 -> 27
  | `R28 -> 28
  | `R29 -> 29
  | `R30 -> 30
  | `R31 -> 31
  | `V0 -> 32
  | `V1 -> 33
  | `V2 -> 34
  | `V3 -> 35
  | `V4 -> 36
  | `V5 -> 37
  | `V6 -> 38
  | `V7 -> 39
  | `V8 -> 40
  | `V9 -> 41
  | `V10 -> 42
  | `V11 -> 43
  | `V12 -> 44
  | `V13 -> 45
  | `V14 -> 46
  | `V15 -> 47
  | `V16 -> 48
  | `V17 -> 49
  | `V18 -> 50
  | `V19 -> 51
  | `V20 -> 52
  | `V21 -> 53
  | `V22 -> 54
  | `V23 -> 55
  | `V24 -> 56
  | `V25 -> 57
  | `V26 -> 58
  | `V27 -> 59
  | `V28 -> 60
  | `V29 -> 61
  | `V30 -> 62
  | `V31 -> 63
  | _ -> failwith "Unknown AArch64 register"
