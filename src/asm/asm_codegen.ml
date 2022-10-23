open Asm
open Mir

let invalid_label_chars = Str.regexp "[-+<>,*():]"

let label_of_mir_label label = Str.global_replace invalid_label_chars "$" label

let get_asm_function_label ~(ir : Program.t) (func : Function.t) : label =
  if func == ir.main_func then
    main_label
  else if func.name == init_func_name then
    init_label
  else
    label_of_mir_label func.name
