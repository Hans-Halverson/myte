open Asm_register

module Register = struct
  type t = Register.t

  type class_ = GeneralClass
end

let all_registers =
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
      (* Link register *)
      `R30;
    ]
