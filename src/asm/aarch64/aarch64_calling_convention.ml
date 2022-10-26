open Asm_calling_convention
open Asm_register

(* AArch64 Procedure Call Standard *)
let aapcs64 =
  object
    inherit calling_convention

    val general_params = [| `R0; `R1; `R2; `R3; `R4; `R5; `R6; `R7 |]

    val float_params = [| `V0; `V1; `V2; `V3; `V4; `V5; `V6; `V7 |]

    val callee_saved_registers =
      RegSet.of_list
        [
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
          `V8;
          `V9;
          `V10;
          `V11;
          `V12;
          `V13;
          `V14;
          `V15;
        ]

    val caller_saved_registers =
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
          `V0;
          `V1;
          `V2;
          `V3;
          `V4;
          `V5;
          `V6;
          `V7;
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

    method general_params = general_params
    method float_params = float_params
    method callee_saved_registers = callee_saved_registers
    method caller_saved_registers = caller_saved_registers

    method calculate_return_register (return_mir_type : Mir_type.Type.t) : Register.t =
      match return_mir_type with
      | Double -> `V0
      | _ -> `R0
  end
