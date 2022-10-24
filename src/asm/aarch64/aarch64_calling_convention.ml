open Asm_calling_convention
open Asm_register

let general_params : Register.t array = [| `R0; `R1; `R2; `R3; `R4; `R5; `R6; `R7 |]

let float_params : Register.t array = [| `V0; `V1; `V2; `V3; `V4; `V5; `V6; `V7 |]

(* AArch64 Procedure Call Standard *)
let aapcs64 =
  object
    inherit calling_convention
    method general_params = general_params
    method float_params = float_params

    method calculate_return_register (return_mir_type : Mir_type.Type.t) : Register.t =
      match return_mir_type with
      | Double -> `XMM0
      | _ -> `A
  end
