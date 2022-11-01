open Asm_instruction_definition

(* Return the opposite of a condition code (NOT CC) *)
let invert_condition_code cc =
  match cc with
  | X86_64.E -> X86_64.NE
  | NE -> E
  | L -> GE
  | G -> LE
  | LE -> G
  | GE -> L
  | B -> AE
  | BE -> A
  | A -> BE
  | AE -> B
  | P -> NP
  | NP -> P

(* Return the condition code that results from swapping the arguments *)
let swap_condition_code_order cc =
  match cc with
  | X86_64.E
  | NE ->
    cc
  | L -> G
  | G -> L
  | LE -> GE
  | GE -> LE
  | B -> A
  | A -> B
  | BE -> AE
  | AE -> BE
  (* Only valid to be swapped because parity flag is only used for ucomisd, where it signals
     whether arguments are "unordered" (at least one of the arguments was NaN). *)
  | P
  | NP ->
    cc

let double_negate_mask_label = "_double_negate_mask"
