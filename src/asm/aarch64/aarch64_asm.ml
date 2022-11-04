open Asm_instruction_definition

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

let register_size_of_subregister_size (subregister_size : AArch64.subregister_size) :
    AArch64.register_size =
  match subregister_size with
  | B
  | H
  | W ->
    Size32
  | X -> Size64

let noop_extend : AArch64.extend = SXTX

let sign_extend_of_subregister_size (subregister_size : AArch64.subregister_size) : AArch64.extend =
  match subregister_size with
  | B -> SXTB
  | H -> SXTH
  | W -> SXTW
  | X -> SXTX
