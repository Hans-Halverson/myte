open Asm_calling_convention
open Asm_register

module SystemVCallingConvention = struct
  let general_params : Register.t array = [| `DI; `SI; `D; `C; `R8; `R9 |]

  let float_params : Register.t array = [| `XMM0; `XMM1; `XMM2; `XMM3; `XMM4; `XMM5; `XMM6; `XMM7 |]

  let num_general_params = Array.length general_params

  let num_float_params = Array.length float_params

  let calculate_param_types = calculate_param_types general_params float_params

  let calculate_return_register (return_mir_type : Mir_type.Type.t) : Register.t =
    match return_mir_type with
    | Double -> `XMM0
    | _ -> `A
end
