open Asm_calling_convention
open Asm_register

(* AArch64 Procedure Call Standard *)
module AAPCS64 = struct
  let general_params : Register.t array = [| `R0; `R1; `R2; `R3; `R4; `R5; `R6; `R7 |]

  let float_params : Register.t array = [| `V0; `V1; `V2; `V3; `V4; `V5; `V6; `V7 |]

  let num_general_params = Array.length general_params

  let num_float_params = Array.length float_params

  let calculate_param_types = calculate_param_types general_params float_params

  let calculate_return_register (return_mir_type : Mir_type.Type.t) : Register.t =
    match return_mir_type with
    | Double -> `XMM0
    | _ -> `A
end
