open Aarch64_instruction_definitions
open Asm
open Asm_instruction_definition

(* Iterate through an array and list, where the list is at least as long as the array *)
let iter2_array_and_list f arr lst =
  let last_idx = Array.length arr - 1 in
  let rec iter i lst =
    if i <= last_idx then
      match lst with
      | hd :: tl ->
        f (Array.unsafe_get arr i) hd;
        iter (i + 1) tl
      | _ -> failwith "List must be as long as the array"
  in
  iter 0 lst

(*
 * ============================
 *            Uses
 * ============================
 *)

let instr_iter_all_operands (instr : Instruction.t) (f : Operand.t -> OperandDef.t -> unit) : unit =
  let instr_def = instr_def instr.instr in
  iter2_array_and_list f instr.operands instr_def.operands
