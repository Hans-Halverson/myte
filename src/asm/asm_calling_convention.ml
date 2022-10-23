open Asm_register

type param_type =
  | ParamInRegister of Register.t
  | ParamOnStack of int

type param_types = param_type array

let calculate_param_types
    (general_params : Register.t array)
    (float_params : Register.t array)
    (param_mir_types : Mir_type.Type.t list) : param_type array =
  let num_general_params = Array.length general_params in
  let num_float_params = Array.length float_params in
  let general_params_used = ref 0 in
  let float_params_used = ref 0 in
  let stack_args_used = ref 0 in
  let mk_stack_type () =
    let next_stack_arg = !stack_args_used in
    stack_args_used := next_stack_arg + 1;
    ParamOnStack next_stack_arg
  in
  List.map
    (fun param_mir_type ->
      match param_mir_type with
      | Mir_type.Type.Double ->
        if !float_params_used >= num_float_params then
          mk_stack_type ()
        else
          let next_param_used = !float_params_used in
          float_params_used := next_param_used + 1;
          ParamInRegister float_params.(next_param_used)
      | _ ->
        if !general_params_used >= num_general_params then
          mk_stack_type ()
        else
          let next_param_used = !general_params_used in
          general_params_used := next_param_used + 1;
          ParamInRegister general_params.(next_param_used))
    param_mir_types
  |> Array.of_list
