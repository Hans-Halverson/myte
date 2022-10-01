open X86_64_builders
open X86_64_instructions
open X86_64_instruction_definitions

class spill_writer ~(get_alias : Operand.t -> Operand.t) =
  object (this)
    method mk_vreg_of_op (op : Operand.t) = mk_virtual_register ~type_:op.type_

    method write_block_spills (block : Block.t) =
      iter_instructions block (fun instruction -> this#visit_instruction instruction)

    method is_memory_value op = Operand.is_memory_value (get_alias op)

    (* Resolve an operand to its alias if one exists *)
    method resolve_operand ~instr op =
      let alias_op = get_alias op in
      (match alias_op.value with
      | MemoryAddress _ -> this#force_registers_in_address ~instr alias_op
      | _ -> ());
      op.value <- alias_op.value

    (* Every memory location (M suffix) must be visited in case it is a Reg that should become a Mem.
       Every MM instruction must make sure that it does not become filled with two memory locations.
       Enforce locations that need a register (R suffix) and ensure they do not become memory locations. *)
    method visit_instruction instr =
      let open Instruction in
      let resolve_operand op = this#resolve_operand ~instr op in
      let add_before_instr new_instr new_operands =
        insert_instruction_before ~before:instr (mk_blockless_instr new_instr new_operands)
      in
      let add_after_instr new_instr new_operands =
        insert_instruction_after ~after:instr (mk_blockless_instr new_instr new_operands)
      in
      match instr.instr with
      (* 64-bit immediates can only be loaded to registers *)
      | MovIM dest_size
        when let imm = cast_to_immediate instr.operands.(0) in
             size_of_immediate imm == Size64
             && Integers.is_out_of_signed_int_range (int64_of_immediate imm) ->
        Array.iter resolve_operand instr.operands;
        let dest_op = instr.operands.(1) in
        if this#is_memory_value dest_op then (
          (* If loaded to a memory location first load immediate to register, then move to memory *)
          let imm_vreg = mk_virtual_register ~type_:Long in
          instr.operands.(1) <- imm_vreg;
          add_after_instr (MovMM dest_size) [| imm_vreg; dest_op |]
        )
      (* Spill instruction with two RegMem operands, only one of which can be a memory. If both
         operands are memorys then src should be converted to a vreg. *)
      | MovMM size
      | AddMM size
      | SubMM size
      | AndMM size
      | OrMM size
      | XorMM size
      | CmpMM size ->
        Array.iter resolve_operand instr.operands;
        let op1 = instr.operands.(0) in
        let op2 = instr.operands.(1) in
        if this#is_memory_value op1 && this#is_memory_value op2 then (
          let op1_vreg = this#mk_vreg_of_op op1 in
          add_before_instr (MovMM size) [| op1; op1_vreg |];
          instr.operands.(0) <- op1_vreg
        )
      (* Spill for commutative operations where the second operand must be a register. If the
         first operand is a register and the second is a memory, order can be switched. Otherwise
         a vreg must be inserted. *)
      | TestMR size ->
        Array.iter resolve_operand instr.operands;
        let op1 = instr.operands.(0) in
        let op2 = instr.operands.(1) in
        if this#is_memory_value op2 then
          if this#is_memory_value op1 then (
            let op2_vreg = this#mk_vreg_of_op op2 in
            add_before_instr (MovMM size) [| op2; op2_vreg |];
            instr.operands.(1) <- op2_vreg
          ) else (
            instr.operands.(0) <- op2;
            instr.operands.(1) <- op1
          )
      (* Standard spilling logic. Resolve all operands, and if a memory operand is used where an
         instruction requires a register then replace with a vreg along with movs to transfer the
         memory value to/from that vreg. *)
      | _ ->
        instr_iteri_all_operands instr (fun i operand operand_def ->
            match operand_def.operand_type with
            | Immediate
            | Label
            | Block ->
              ()
            | MemoryAddress -> this#force_registers_in_address ~instr operand
            | RegMem -> resolve_operand operand
            | Register ->
              resolve_operand operand;
              if this#is_memory_value operand then (
                let size = instr_operand_size instr.instr i in
                let vreg = this#mk_vreg_of_op operand in
                instr.operands.(i) <- vreg;
                if operand_is_use operand_def then add_before_instr (MovMM size) [| operand; vreg |];
                if operand_is_def operand_def then add_after_instr (MovMM size) [| vreg; operand |]
              ))

    (* The base and offset in memory addresses must be a register. If the base or offset has been
       resolved to a stack slot, emit a mov to copy this stack slot to a register and use the
       register in the memory address instead. *)
    method force_registers_in_address ~instr op =
      let addr = cast_to_memory_address op in
      let add_before_instr new_instr new_operands =
        insert_instruction_before ~before:instr (mk_blockless_instr new_instr new_operands)
      in
      (match addr.base with
      | RegBase base_reg ->
        this#resolve_operand ~instr base_reg;
        if this#is_memory_value base_reg then (
          let vreg = this#mk_vreg_of_op base_reg in
          add_before_instr (MovMM Size64) [| base_reg; vreg |];
          addr.base <- RegBase vreg
        )
      | _ -> ());
      match addr.index_and_scale with
      | Some (scale_reg, scale) ->
        this#resolve_operand ~instr scale_reg;
        if this#is_memory_value scale_reg then (
          let vreg = this#mk_vreg_of_op scale_reg in
          add_before_instr (MovMM Size64) [| scale_reg; vreg |];
          addr.index_and_scale <- Some (vreg, scale)
        ) else
          ()
      | _ -> ()
  end
