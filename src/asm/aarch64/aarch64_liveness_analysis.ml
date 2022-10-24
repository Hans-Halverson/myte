open Aarch64_builders
open Aarch64_register
open Asm
open Asm_instruction_definition
open Asm_register

class virtual use_def_visitor color_to_representative_operand =
  object (this)
    inherit Asm_liveness_analysis.regs_use_def_visitor

    val mutable prev_blocks = BlockMMap.empty

    method prev_blocks = prev_blocks

    method get_representative_register reg = RegMap.find reg color_to_representative_operand

    method mark_block_edge (instr : Instruction.t) =
      match instr with
      | { instr = `B; operands = [| { value = Block next_block; _ } |]; block; _ } ->
        prev_blocks <- BlockMMap.add next_block block prev_blocks
      | _ -> ()

    method visit_instruction (instr : Instruction.t) =
      match instr with
      (* Return without argument implicitly uses link register *)
      | { instr = `Ret; _ } ->
        this#visit_register_use ~instr (this#get_representative_register lr);
        this#visit_explicit_uses_and_defs instr
      | _ -> this#visit_explicit_uses_and_defs instr

    method visit_explicit_uses_and_defs instr =
      let resolve_register op =
        match op.Operand.value with
        | PhysicalRegister reg -> Some (this#get_representative_register reg)
        | VirtualRegister -> Some op
        | _ -> None
      in

      (* First visit uses then defs *)
      instr_iter_all_operands instr (fun operand operand_def ->
          if operand_is_use operand_def then
            match resolve_register operand with
            | Some reg -> this#visit_register_use ~instr reg
            | None -> ());

      instr_iter_all_operands instr (fun operand operand_def ->
          if operand_is_def operand_def then
            match resolve_register operand with
            | Some reg -> this#visit_register_def ~instr reg
            | None -> ())
  end

class regs_liveness_analyzer (func : Function.t) color_to_operand =
  object
    inherit use_def_visitor color_to_operand
    inherit Asm_liveness_analysis.regs_liveness_analyzer func
  end
