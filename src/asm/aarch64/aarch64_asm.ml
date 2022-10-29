open Asm_instruction_definition

let mk_data_section () = Array.make 5 []

(* Return the opposite of a condition code (NOT cond) *)
let invert_cond cond =
  match cond with
  | AArch64.EQ -> AArch64.NE
  | NE -> EQ
  | LT -> GE
  | GT -> LE
  | LE -> GT
  | GE -> LT

(* Return the condition code that results from swapping the arguments *)
let swap_cond_order cond =
  match cond with
  | AArch64.EQ
  | NE ->
    cond
  | LT -> GT
  | GT -> LT
  | LE -> GE
  | GE -> LE
