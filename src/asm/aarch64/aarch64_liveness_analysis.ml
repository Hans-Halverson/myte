open Aarch64_builders
open Aarch64_register
open Asm
open Asm_builders
open Asm_calling_convention
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
      | { instr = `B | `BCond _; operands = [| { value = Block next_block; _ } |]; block; _ }
      | { instr = `Cbz _; operands = [| _; { value = Block next_block; _ } |]; block; _ } ->
        prev_blocks <- BlockMMap.add next_block block prev_blocks
      | _ -> ()

    method visit_instruction (instr : Instruction.t) =
      match instr with
      (* Calls implicitly use all parameter registers and define all caller save registers *)
      | { instr = `BL (param_types, calling_convention); _ }
      | { instr = `BLR (param_types, calling_convention); _ } ->
        Array.iter
          (fun param_type ->
            match param_type with
            | ParamInRegister reg ->
              this#visit_register_use ~instr (this#get_representative_register reg)
            | _ -> ())
          param_types;
        RegSet.iter
          (fun reg -> this#visit_register_def ~instr (this#get_representative_register reg))
          calling_convention#caller_saved_registers;
        this#visit_explicit_uses_and_defs instr
      (* Return without argument implicitly uses link register *)
      | { instr = `Ret; _ } ->
        this#visit_register_use ~instr (this#get_representative_register lr);
        this#visit_explicit_uses_and_defs instr
      | _ -> this#visit_explicit_uses_and_defs instr

    method visit_explicit_uses_and_defs instr =
      let resolve_register op =
        match op.Operand.value with
        | PhysicalRegister (`ZR | `SP) -> None
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

class vslots_liveness_analyzer (func : Function.t) =
  object (this)
    inherit Asm_liveness_analysis.liveness_analyzer func

    val mutable prev_blocks = BlockMMap.empty
    val mutable use_blocks = OBMMap.empty
    val mutable def_blocks = OBMMap.empty
    val mutable use_before_def_blocks = OBMMap.empty

    method prev_blocks = prev_blocks
    method use_blocks = use_blocks
    method def_blocks = def_blocks
    method use_before_def_blocks = use_before_def_blocks

    method init () =
      func_iter_blocks func (fun block -> iter_instructions block this#visit_instruction)

    method visit_block block = iter_instructions block this#visit_instruction

    method visit_instruction instr =
      (* Mark block edges *)
      (match instr with
      | { instr = `B | `BCond _; operands = [| { value = Block next_block; _ } |]; block; _ }
      | { instr = `Cbz _; operands = [| _; { value = Block next_block; _ } |]; block; _ } ->
        prev_blocks <- BlockMMap.add next_block block prev_blocks
      | _ -> ());

      (* After register allocation vslots will only apper as the second argument of spill instructions *)
      match instr.instr with
      | `SpillUse _ ->
        let operand = instr.operands.(1) in
        if operand.value == VirtualStackSlot then
          use_blocks <- OBMMap.add operand instr.block use_blocks
      | `SpillDef _ ->
        let operand = instr.operands.(1) in
        if operand.value == VirtualStackSlot then (
          let block = instr.block in
          if
            OBMMap.contains operand block use_blocks
            && not (OBMMap.contains operand block def_blocks)
          then
            use_before_def_blocks <- OBMMap.add operand block use_before_def_blocks;
          def_blocks <- OBMMap.add operand block def_blocks
        )
      | _ -> ()
  end
