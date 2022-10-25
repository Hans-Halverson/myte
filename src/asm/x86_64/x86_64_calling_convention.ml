open Asm_calling_convention
open Asm_register

let system_v_calling_convention =
  object
    inherit calling_convention

    val general_params = [| `DI; `SI; `D; `C; `R8; `R9 |]

    val float_params = [| `XMM0; `XMM1; `XMM2; `XMM3; `XMM4; `XMM5; `XMM6; `XMM7 |]

    val callee_saved_registers = RegSet.of_list [`B; `SP; `BP; `R12; `R13; `R14; `R15]

    val caller_saved_registers =
      RegSet.of_list
        [
          `A;
          `C;
          `D;
          `SI;
          `DI;
          `R8;
          `R9;
          `R10;
          `R11;
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

    method general_params = general_params
    method float_params = float_params
    method callee_saved_registers = callee_saved_registers
    method caller_saved_registers = caller_saved_registers

    method calculate_return_register (return_mir_type : Mir_type.Type.t) : Register.t =
      match return_mir_type with
      | Double -> `XMM0
      | _ -> `A
  end
