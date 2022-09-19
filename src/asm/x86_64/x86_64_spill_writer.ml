open X86_64_builders
open X86_64_instructions

class spill_writer ~(get_alias : Operand.t -> Operand.t) =
  object (this)
    method mk_vreg_of_op (op : Operand.t) = mk_virtual_register ~type_:op.type_

    method write_block_spills (block : Block.t) =
      iter_instructions block (fun instruction -> this#visit_instruction ~block instruction)

    method is_memory_value op = Operand.is_memory_value (get_alias op)

    (* Resolve an operand to its alias if one exists *)
    method resolve_operator ~block ~instr op =
      let alias_op = get_alias op in
      (match alias_op.value with
      | MemoryAddress _ -> this#force_registers_in_address ~block ~instr alias_op
      | _ -> ());
      op.value <- alias_op.value

    (* Every memory location (M suffix) must be visited in case it is a Reg that should become a Mem.
       Every MM instruction must make sure that it does not become filled with two memory locations.
       Enforce locations that need a register (R suffix) and ensure they do not become memory locations. *)
    method visit_instruction ~block instr =
      let open Instruction in
      let replace_instr new_instr = instr.instr <- new_instr in
      let add_before_instr new_instr =
        insert_instruction_before ~before:instr (mk_blockless_instr new_instr)
      in
      let add_after_instr new_instr =
        insert_instruction_after ~after:instr (mk_blockless_instr new_instr)
      in
      let mk_vreg_of_op op = this#mk_vreg_of_op op in
      let resolve_operator op = this#resolve_operator ~block ~instr op in
      (* Must have a register not a memory address - if necessary create new register then move *)
      let force_register_write size op f =
        resolve_operator op;
        if this#is_memory_value op then (
          let vreg_dest = mk_vreg_of_op op in
          replace_instr (f vreg_dest);
          add_after_instr (MovMM (size, vreg_dest, op))
        ) else
          replace_instr (f op)
      in
      let force_register_read size op f =
        resolve_operator op;
        if this#is_memory_value op then (
          let vreg_dest = mk_vreg_of_op op in
          add_before_instr (MovMM (size, op, vreg_dest));
          replace_instr (f vreg_dest)
        ) else
          replace_instr (f op)
      in
      let force_register_read_write size op f =
        resolve_operator op;
        if this#is_memory_value op then (
          let vreg_dest = mk_vreg_of_op op in
          add_before_instr (MovMM (size, op, vreg_dest));
          replace_instr (f vreg_dest);
          add_after_instr (MovMM (size, vreg_dest, op))
        ) else
          replace_instr (f op)
      in
      let resolve_binop_single_mem size src_op dest_op f =
        resolve_operator src_op;
        resolve_operator dest_op;
        if this#is_memory_value src_op && this#is_memory_value dest_op then (
          let src_vreg = mk_vreg_of_op src_op in
          add_before_instr (MovMM (size, src_op, src_vreg));
          replace_instr (f src_vreg dest_op)
        ) else
          replace_instr (f src_op dest_op)
      in
      match instr.instr with
      (* 64-bit immediates can only be loaded to registers *)
      | MovIM (dest_size, ({ value = Immediate (Imm64 i); _ } as imm), dest_op)
        when Integers.is_out_of_signed_int_range i ->
        resolve_operator dest_op;
        if Operand.is_reg_value dest_op then
          replace_instr (MovIM (dest_size, imm, dest_op))
        else
          (* If loaded to a memory location first load immediate to register, then move to memory *)
          let new_vreg = mk_virtual_register ~type_:Long in
          add_before_instr (MovIM (dest_size, imm, new_vreg));
          replace_instr (MovMM (dest_size, new_vreg, dest_op))
      (* TestMR must have a register as its second operand *)
      | TestMR (size, op1, op2) ->
        resolve_operator op1;
        resolve_operator op2;
        if Operand.is_reg_value op2 then
          ()
        else if Operand.is_reg_value op1 then
          (* If register was resolved to memory location but memory was resolved to register, swap *)
          replace_instr (TestMR (size, op2, op1))
        else
          (* If both operands are memory location, create and move to new vreg for second operand *)
          let new_vreg = mk_vreg_of_op op2 in
          add_before_instr (MovMM (size, op2, new_vreg));
          replace_instr (TestMR (size, op1, new_vreg))
      (* All instructions that allow register and memory operands need their operand resolved but
         can be kept. *)
      | PushM op
      | PopM op
      | MovIM (_, _, op)
      | NegM (_, op)
      | AddIM (_, _, op)
      | SubIM (_, _, op)
      | AndIM (_, _, op)
      | OrIM (_, _, op)
      | XorIM (_, _, op)
      | ShlI (_, _, op)
      | ShrI (_, _, op)
      | SarI (_, _, op)
      | ShlR (_, op)
      | ShrR (_, op)
      | SarR (_, op)
      | CmpMI (_, op, _)
      | IDiv (_, op)
      | NotM (_, op)
      | SetCC (_, op)
      | CallM (_, op) ->
        (* Resolve all instructions with a single operand that must be a register *)
        resolve_operator op
      | IMulMIR (size, src_op, src_imm, dest_reg) ->
        resolve_operator src_op;
        force_register_write
          (size_of_immediate (cast_to_immediate src_imm))
          dest_reg
          (fun dest_reg' -> IMulMIR (size, src_op, src_imm, dest_reg'))
      | MovSX (src_size, dest_size, src_op, dest_reg) ->
        resolve_operator src_op;
        force_register_write dest_size dest_reg (fun reg' ->
            MovSX (src_size, dest_size, src_op, reg'))
      | MovZX (src_size, dest_size, src_op, dest_reg) ->
        resolve_operator src_op;
        force_register_write dest_size dest_reg (fun reg' ->
            MovZX (src_size, dest_size, src_op, reg'))
      | Lea (size, addr, reg) ->
        this#force_registers_in_address ~block ~instr addr;
        force_register_write size reg (fun reg' -> Lea (size, addr, reg'))
      (* Resolve all operations with two operands only one of which can be a memory *)
      | MovMM (size, src_op, dest_op) ->
        resolve_binop_single_mem size src_op dest_op (fun s d -> MovMM (size, s, d))
      (* Destination must be register for floating point operations *)
      | AddMM (size, src_op, dest_op) when dest_op.type_ = Double ->
        resolve_operator src_op;
        force_register_read_write size dest_op (fun dest_op' -> AddMM (size, src_op, dest_op'))
      | AddMM (size, src_op, dest_op) ->
        resolve_binop_single_mem size src_op dest_op (fun s d -> AddMM (size, s, d))
        (* Destination must be register for floating point operations *)
      | SubMM (size, src_op, dest_op) when dest_op.type_ = Double ->
        resolve_operator src_op;
        force_register_read_write size dest_op (fun dest_op' -> SubMM (size, src_op, dest_op'))
      | SubMM (size, src_op, dest_op) ->
        resolve_binop_single_mem size src_op dest_op (fun s d -> SubMM (size, s, d))
      | AndMM (size, src_op, dest_op) ->
        resolve_binop_single_mem size src_op dest_op (fun s d -> AndMM (size, s, d))
      | OrMM (size, src_op, dest_op) ->
        resolve_binop_single_mem size src_op dest_op (fun s d -> OrMM (size, s, d))
      | XorMM (Size128, src_op, dest_op) ->
        resolve_operator src_op;
        (* Only 8 bytes need to be moved if a Mov is generated since only doubles are supported  *)
        force_register_read_write Size64 dest_op (fun dest_op' -> XorMM (Size128, src_op, dest_op'))
      | XorMM (size, src_op, dest_op) ->
        resolve_binop_single_mem size src_op dest_op (fun s d -> XorMM (size, s, d))
      | CmpMM (size, src_op, dest_op) ->
        if src_op.type_ == Double then (
          resolve_operator dest_op;
          force_register_read size src_op (fun src_op' -> CmpMM (size, src_op', dest_op))
        ) else
          resolve_binop_single_mem size src_op dest_op (fun s d -> CmpMM (size, s, d))
      | MulMR (size, src_op, dest_op) ->
        resolve_operator src_op;
        force_register_read_write size dest_op (fun dest_op' -> MulMR (size, src_op, dest_op'))
      | FDivMR (size, src_op, dest_op) ->
        resolve_operator src_op;
        force_register_read_write size dest_op (fun dest_op' -> FDivMR (size, src_op, dest_op'))
      | ConvertIntToFloat (size, src_op, dest_op) ->
        force_register_write size dest_op (fun dest_op' ->
            ConvertIntToFloat (size, src_op, dest_op'))
      | ConvertFloatToInt (size, src_op, dest_op) ->
        force_register_write size dest_op (fun dest_op' ->
            ConvertFloatToInt (size, src_op, dest_op'))
      (* Instructions with no register/memories *)
      | PushI _
      | ConvertDouble _
      | Jmp _
      | JmpCC _
      | CallL _
      | Leave
      | Ret
      | Syscall ->
        ()

    (* The base and offset in memory addresses must be a register. If the base or offset has been
       resolved to a stack slot, emit a mov to copy this stack slot to a register and use the
       register in the memory address instead. *)
    method force_registers_in_address ~block ~instr op =
      let addr = cast_to_memory_address op in
      let { MemoryAddress.offset; base; index_and_scale } = addr in
      let add_before_instr new_instr =
        insert_instruction_before ~before:instr (mk_blockless_instr new_instr)
      in
      let resolve_operator op = this#resolve_operator ~block ~instr op in
      let base' =
        match base with
        | RegBase base_reg ->
          resolve_operator base_reg;
          if this#is_memory_value base_reg then (
            let vreg = this#mk_vreg_of_op base_reg in
            add_before_instr (MovMM (Size64, base_reg, vreg));
            MemoryAddress.RegBase vreg
          ) else
            base
        | _ -> base
      in
      let index_and_scale' =
        match index_and_scale with
        | Some (scale_reg, scale) ->
          resolve_operator scale_reg;
          if this#is_memory_value scale_reg then (
            let vreg = this#mk_vreg_of_op scale_reg in
            add_before_instr (MovMM (Size64, scale_reg, vreg));
            Some (vreg, scale)
          ) else
            index_and_scale
        | _ -> index_and_scale
      in
      if base != base' || index_and_scale != index_and_scale' then
        op.value <- MemoryAddress { offset; base = base'; index_and_scale = index_and_scale' }
  end
