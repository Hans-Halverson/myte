open Asm_register

type param_type =
  | ParamInRegister of Register.t
  | ParamOnStack of int

type param_types = param_type array
