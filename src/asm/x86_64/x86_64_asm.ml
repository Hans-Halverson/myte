open Asm
open Asm_instruction_definition

let pointer_size = 8

let mk_data_section () = Array.make 5 []

let rec align_of_data_value d =
  match d with
  | ImmediateData imm -> bytes_of_size (size_of_immediate imm)
  | AsciiData _ -> 1
  | LabelData _ -> 8
  | SSELiteral _ -> 16
  | ArrayData data ->
    List.fold_left
      (fun max_align value ->
        let align = align_of_data_value value in
        if align > max_align then
          align
        else
          max_align)
      1
      data

let align_to_data_section_align_index align =
  match align with
  | 1 -> 0
  | 2 -> 1
  | 4 -> 2
  | 8 -> 3
  | 16 -> 4
  | _ -> failwith "Invalid alignment"

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
