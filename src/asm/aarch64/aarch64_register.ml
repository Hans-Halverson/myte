open Asm_register

module Register = struct
  type t = Register.t

  type class_ = GeneralClass
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

let all_registers = RegSet.union general_purpose_registers vector_registers

(* R31 is SP or ZR depending on context *)
let sp : Register.t = `R31
let zr : Register.t = `R31
