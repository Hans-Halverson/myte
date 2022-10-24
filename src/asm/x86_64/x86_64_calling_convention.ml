open Asm_calling_convention
open Asm_register

let general_params : Register.t array = [| `DI; `SI; `D; `C; `R8; `R9 |]

let float_params : Register.t array = [| `XMM0; `XMM1; `XMM2; `XMM3; `XMM4; `XMM5; `XMM6; `XMM7 |]

let system_v_calling_convention =
  object
    inherit calling_convention

    method general_params = general_params
    method float_params = float_params

    method calculate_return_register (return_mir_type : Mir_type.Type.t) : Register.t =
      match return_mir_type with
      | Double -> `XMM0
      | _ -> `A
  end
