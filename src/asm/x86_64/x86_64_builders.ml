open Asm
open Asm_instruction_definition
open X86_64_instruction_definitions

(*
 * ============================
 *            Uses
 * ============================
 *)

let rec instr_iter_all_operands (instr : Instruction.t) (f : Operand.t -> OperandDef.t -> unit) :
    unit =
  let instr_def = instr_def instr.instr in
  List.iteri
    (fun i (operand_def : OperandDef.t) ->
      let operand = instr.operands.(i) in
      f operand operand_def)
    instr_def.operands

and instr_iteri_all_operands (instr : Instruction.t) (f : int -> Operand.t -> OperandDef.t -> unit)
    : unit =
  let instr_def = instr_def instr.instr in
  List.iteri
    (fun i (operand_def : OperandDef.t) ->
      let operand = instr.operands.(i) in
      f i operand operand_def)
    instr_def.operands

(* Iterate over all register or memory operands in an instruction. Do not iterate over memory
   addresses directly, instead iterate over their register base and index components. *)
and instr_iter_reg_mem_operands (instr : Instruction.t) (f : Operand.t -> OperandDef.t -> unit) :
    unit =
  instr_iter_all_operands instr (fun operand operand_def ->
      operand_iter_reg_mem_operands operand operand_def f)

(* Iterate over all register or memory operands in an operand. Do not iterate over memory
   addresses directly, instead iterate over their register base and index components. *)
and operand_iter_reg_mem_operands
    (operand : Operand.t) (operand_def : OperandDef.t) (f : Operand.t -> OperandDef.t -> unit) :
    unit =
  match operand.value with
  | X86_64_MemoryAddress { offset = _; base; index_and_scale } ->
    (match base with
    | RegBase base_reg -> f base_reg address_reg_operand_def
    | NoBase
    | IPBase ->
      ());
    (match index_and_scale with
    | Some (index_reg, _) -> f index_reg address_reg_operand_def
    | None -> ())
  | PhysicalRegister _
  | VirtualRegister
  | StackSlot _
  | VirtualStackSlot ->
    f operand operand_def
  | Immediate _
  | Function _
  | Block _
  | Label _ ->
    ()
