open Asm
open Asm_builders
open Asm_instruction_definition
open Aarch64_builders
open Aarch64_instruction_definitions

class spill_writer ~(get_alias : Operand.t -> Operand.t) =
  object (this)
    method write_block_spills (block : Block.t) =
      iter_instructions block (fun instruction ->
          match instruction.instr with
          | `SpillUse _
          | `SpillDef _ ->
            ()
          | _ -> this#visit_instruction instruction)

    (* Resolve an operand to its alias if one exists *)
    method resolve_operand op =
      let alias_op = get_alias op in
      op.value <- alias_op.value

    method visit_instruction instr =
      let add_before_instr new_instr new_operands =
        insert_instruction_before ~before:instr (mk_blockless_instr new_instr new_operands)
      in
      let add_after_instr new_instr new_operands =
        insert_instruction_after ~after:instr (mk_blockless_instr new_instr new_operands)
      in
      (* Standard spilling logic. Resolve all operands, and if a memory operand is used where an
         instruction requires a register then replace with a vreg along with movs to transfer the
         memory value to/from that vreg. *)
      instr_iteri_all_operands instr (fun i operand operand_def ->
          match operand_def.operand_type with
          | Immediate
          | Function
          | Block
          | Label ->
            ()
          | Register ->
            this#resolve_operand operand;
            if Operand.is_memory_value operand then (
              let subregister_size =
                Aarch64_codegen.subregister_size_of_mir_value_type operand.type_
              in
              let vreg = mk_virtual_register ~type_:operand.type_ in
              vreg.from_spill <- true;
              instr.operands.(i) <- vreg;
              (if operand_is_use operand_def then
                let size = instr_register_size instr.instr i in
                add_before_instr (`SpillUse (size, subregister_size, true)) [| vreg; operand |]);
              if operand_is_def operand_def then
                add_after_instr (`SpillDef subregister_size) [| vreg; operand |]
            )
          | RegMem
          | MemoryAddress ->
            failwith "Operand cannot appear in AArch64")
  end
