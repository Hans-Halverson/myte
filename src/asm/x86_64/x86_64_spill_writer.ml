open X86_64_gen_context
open X86_64_instructions

class spill_writer ~(gcx : Gcx.t) ~(get_alias : Operand.t -> Operand.t) =
  object (this)
    val mutable current_instruction_builder = []

    val mutable current_block_builders = []

    method mk_vreg () = Operand.mk_virtual_register ()

    method add_instr instr = current_instruction_builder <- instr :: current_instruction_builder

    method write_block_spills (block : Block.t) =
      current_block_builders <- [];
      List.iter
        (fun instruction ->
          current_instruction_builder <- [];
          this#visit_instruction ~block instruction;
          current_block_builders <- List.rev current_instruction_builder :: current_block_builders)
        block.instructions;
      block.instructions <- List.rev current_block_builders |> List.flatten

    method is_memory_value op = Operand.is_memory_value (get_alias op)

    (* Every memory location (M suffix) must be visited in case it is a Reg that should become a Mem.
       Every MM instruction must make sure that it does not become filled with two memory locations.
       Enforce locations that need a register (R suffix) and ensure they do not become memory locations. *)
    method visit_instruction ~block instr =
      let open Instruction in
      let (instr_id, instr) = instr in
      let mk_single instr = this#add_instr (instr_id, instr) in
      let mk_vreg () = this#mk_vreg () in
      let resolve_op mem = this#resolve_op ~block mem in
      let resolve_binop_single_mem size src_mem dest_mem f =
        let src_mem = resolve_op src_mem in
        let dest_mem = resolve_op dest_mem in
        if this#is_memory_value src_mem && this#is_memory_value dest_mem then (
          let src_vreg = mk_vreg () in
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, src_mem, src_vreg));
          this#add_instr (instr_id, f src_vreg dest_mem)
        ) else
          mk_single (f src_mem dest_mem)
      in
      (* Must have a register not a memory address - if necessary create new register then move *)
      let force_register_write size op f =
        let op = resolve_op op in
        if this#is_memory_value op then (
          let vreg_dest = mk_vreg () in
          this#add_instr (instr_id, f vreg_dest);
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, vreg_dest, op))
        ) else
          mk_single (f op)
      in
      match instr with
      | PushM mem -> mk_single (PushM (resolve_op mem))
      | PopM mem -> mk_single (PopM (resolve_op mem))
      (* 64-bit immediates can only be loaded to registers *)
      | MovIM (dest_size, (Imm64 i as imm), dest_mem) when Integers.is_out_of_signed_int_range i ->
        let dest_mem = resolve_op dest_mem in
        if Operand.is_reg_value dest_mem then
          mk_single (MovIM (dest_size, imm, dest_mem))
        else
          (* If loaded to a memory location first load immediate to register, then move to memory *)
          let new_vreg = mk_vreg () in
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovIM (dest_size, imm, new_vreg));
          this#add_instr (instr_id, MovMM (dest_size, new_vreg, dest_mem))
      | MovIM (size, src_imm, dest_mem) -> mk_single (MovIM (size, src_imm, resolve_op dest_mem))
      | MovMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> MovMM (size, s, d))
      | MovSX (src_size, dest_size, src_mem, dest_reg) ->
        force_register_write dest_size dest_reg (fun reg' ->
            MovSX (src_size, dest_size, resolve_op src_mem, reg'))
      | MovZX (src_size, dest_size, src_mem, dest_reg) ->
        force_register_write dest_size dest_reg (fun reg' ->
            MovZX (src_size, dest_size, resolve_op src_mem, reg'))
      | Lea (size, addr, reg) ->
        force_register_write size reg (fun reg' ->
            Lea (size, this#force_registers_in_address ~block addr, reg'))
      | NegM (size, mem) -> mk_single (NegM (size, resolve_op mem))
      | AddIM (size, src_imm, dest_mem) -> mk_single (AddIM (size, src_imm, resolve_op dest_mem))
      | AddMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> AddMM (size, s, d))
      | SubIM (size, src_imm, dest_mem) -> mk_single (SubIM (size, src_imm, resolve_op dest_mem))
      | SubMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> SubMM (size, s, d))
      | IMulMR (size, src_mem, dest_reg) -> mk_single (IMulMR (size, resolve_op src_mem, dest_reg))
      | IMulMIR (size, src_mem, src_imm, dest_reg) ->
        force_register_write (size_of_immediate src_imm) dest_reg (fun dest_reg' ->
            IMulMIR (size, resolve_op src_mem, src_imm, dest_reg'))
      | IDiv (size, mem) -> mk_single (IDiv (size, resolve_op mem))
      | NotM (size, mem) -> mk_single (NotM (size, resolve_op mem))
      | AndIM (size, src_imm, dest_mem) -> mk_single (AndIM (size, src_imm, resolve_op dest_mem))
      | AndMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> AndMM (size, s, d))
      | OrIM (size, src_imm, dest_mem) -> mk_single (OrIM (size, src_imm, resolve_op dest_mem))
      | OrMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> OrMM (size, s, d))
      | XorIM (size, src_imm, dest_mem) -> mk_single (XorIM (size, src_imm, resolve_op dest_mem))
      | XorMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> XorMM (size, s, d))
      | ShlI (size, src_imm, dest_mem) -> mk_single (ShlI (size, src_imm, resolve_op dest_mem))
      | ShlR (size, dest_mem) -> mk_single (ShlR (size, resolve_op dest_mem))
      | ShrI (size, src_imm, dest_mem) -> mk_single (ShrI (size, src_imm, resolve_op dest_mem))
      | ShrR (size, dest_mem) -> mk_single (ShrR (size, resolve_op dest_mem))
      | SarI (size, src_imm, dest_mem) -> mk_single (SarI (size, src_imm, resolve_op dest_mem))
      | SarR (size, dest_mem) -> mk_single (SarR (size, resolve_op dest_mem))
      | CmpMI (size, mem, imm) -> mk_single (CmpMI (size, resolve_op mem, imm))
      | CmpMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> CmpMM (size, s, d))
      | TestMR (size, arg1, arg2) ->
        let arg1 = resolve_op arg1 in
        let arg2 = resolve_op arg2 in
        if Operand.is_reg_value arg2 then
          mk_single (TestMR (size, arg1, arg2))
        else if Operand.is_reg_value arg1 then
          (* If register was resolved to memory location but memory was resolved to register, swap *)
          mk_single (TestMR (size, arg2, arg1))
        else
          (* If both arguments are memory location, create and move to new vreg for second argument *)
          let new_vreg = mk_vreg () in
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, arg2, new_vreg));
          this#add_instr (instr_id, TestMR (size, arg1, new_vreg))
      | SetCC (cc, mem) -> mk_single (SetCC (cc, resolve_op mem))
      | CallM (size, mem) -> mk_single (CallM (size, resolve_op mem))
      | PushI _
      | ConvertDouble _
      | Jmp _
      | JmpCC _
      | CallL _
      | Leave
      | Ret
      | Syscall ->
        mk_single instr

    (* Resolve an operand to its alias if one exists *)
    method resolve_op ~block op =
      let op = get_alias op in
      (match op.value with
      | MemoryAddress mem -> op.value <- MemoryAddress (this#force_registers_in_address ~block mem)
      | _ -> ());
      op

    (* The base and offset in memory addresses must be a register. If the base or offset has been
       resolved to a stack slot, emit a mov to copy this stack slot to a register and use the
       register in the memory address instead. *)
    method force_registers_in_address ~block addr =
      let { MemoryAddress.offset; base; index_and_scale } = addr in
      let base' =
        match base with
        | RegBase base_reg ->
          let base_reg = this#resolve_op ~block base_reg in
          if this#is_memory_value base_reg then (
            let vreg = this#mk_vreg () in
            this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Size64, base_reg, vreg));
            MemoryAddress.RegBase vreg
          ) else
            base
        | _ -> base
      in
      let index_and_scale' =
        match index_and_scale with
        | Some (scale_reg, scale) ->
          let scale_reg = this#resolve_op ~block scale_reg in
          if this#is_memory_value scale_reg then (
            let vreg = this#mk_vreg () in
            this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Size64, scale_reg, vreg));
            Some (vreg, scale)
          ) else
            index_and_scale
        | _ -> index_and_scale
      in
      if base == base' && index_and_scale == index_and_scale' then
        addr
      else
        { offset; base = base'; index_and_scale = index_and_scale' }
  end
