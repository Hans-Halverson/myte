open X86_64_gen_context
open X86_64_instructions

class spill_writer ~gcx =
  object (this)
    val mutable new_vregs = VRegSet.empty

    val mutable current_instruction_builder = []

    val mutable current_block_builders = []

    method new_vregs = new_vregs

    method mk_vreg () =
      let vreg = VReg.mk ~resolution:Unresolved in
      new_vregs <- VRegSet.add vreg new_vregs;
      vreg

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

    (* Every memory location (M suffix) must be visited in case it is a Reg that should become a Mem.
       Every MM instruction must make sure that it does not become filled with two memory locations.
       Enforce locations that need a register (R suffix) and ensure they do not become memory locations. *)
    method visit_instruction ~block instr =
      let open Instruction in
      let (instr_id, instr) = instr in
      let mk_single instr = this#add_instr (instr_id, instr) in
      let mk_vreg () = this#mk_vreg () in
      let resolve_reg mem = this#resolve_reg ~block mem in
      let resolve_binop_single_mem size src_mem dest_mem f =
        let src_mem = resolve_reg src_mem in
        let dest_mem = resolve_reg dest_mem in
        if VReg.is_memory_value src_mem && VReg.is_memory_value dest_mem then (
          let src_vreg = mk_vreg () in
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, src_mem, src_vreg));
          this#add_instr (instr_id, f src_vreg dest_mem)
        ) else
          mk_single (f src_mem dest_mem)
      in
      (* Must have a register not a memory address - if necessary create new register then move *)
      let force_register_write size reg f =
        let vreg = resolve_reg reg in
        if VReg.is_memory_value vreg then (
          let vreg_dest = mk_vreg () in
          this#add_instr (instr_id, f vreg_dest);
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, vreg_dest, vreg))
        ) else
          mk_single (f reg)
      in
      match instr with
      | PushM mem -> mk_single (PushM (resolve_reg mem))
      | PopM mem -> mk_single (PopM (resolve_reg mem))
      (* 64-bit immediates can only be loaded to registers *)
      | MovIM (dest_size, (Imm64 i as imm), dest_mem) when Integers.is_out_of_signed_int_range i ->
        let dest_value = resolve_reg dest_mem in

        (* If already loaded to a vreg, force that vreg to be a register *)
        if VReg.is_reg_value dest_value then
          force_register_write dest_size dest_value (fun reg' -> MovIM (dest_size, imm, reg'))
        else
          (* If loaded to a memory location first load immediate to register, then move to memory *)
          let new_vreg = mk_vreg () in
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovIM (dest_size, imm, new_vreg));
          this#add_instr (instr_id, MovMM (dest_size, new_vreg, dest_value))
      | MovIM (size, src_imm, dest_mem) -> mk_single (MovIM (size, src_imm, resolve_reg dest_mem))
      | MovMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> MovMM (size, s, d))
      | MovSX (src_size, dest_size, src_mem, dest_reg) ->
        force_register_write dest_size dest_reg (fun reg' ->
            MovSX (src_size, dest_size, resolve_reg src_mem, reg'))
      | MovZX (src_size, dest_size, src_mem, dest_reg) ->
        force_register_write dest_size dest_reg (fun reg' ->
            MovZX (src_size, dest_size, resolve_reg src_mem, reg'))
      | Lea (size, addr, reg) ->
        force_register_write size reg (fun reg' ->
            Lea (size, this#force_registers_in_address ~block addr, reg'))
      | NegM (size, mem) -> mk_single (NegM (size, resolve_reg mem))
      | AddIM (size, src_imm, dest_mem) -> mk_single (AddIM (size, src_imm, resolve_reg dest_mem))
      | AddMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> AddMM (size, s, d))
      | SubIM (size, src_imm, dest_mem) -> mk_single (SubIM (size, src_imm, resolve_reg dest_mem))
      | SubMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> SubMM (size, s, d))
      | IMulMR (size, src_mem, dest_reg) -> mk_single (IMulMR (size, resolve_reg src_mem, dest_reg))
      | IMulMIR (size, src_mem, src_imm, dest_reg) ->
        force_register_write (size_of_immediate src_imm) dest_reg (fun dest_reg' ->
            IMulMIR (size, resolve_reg src_mem, src_imm, dest_reg'))
      | IDiv (size, mem) -> mk_single (IDiv (size, resolve_reg mem))
      | NotM (size, mem) -> mk_single (NotM (size, resolve_reg mem))
      | AndIM (size, src_imm, dest_mem) -> mk_single (AndIM (size, src_imm, resolve_reg dest_mem))
      | AndMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> AndMM (size, s, d))
      | OrIM (size, src_imm, dest_mem) -> mk_single (OrIM (size, src_imm, resolve_reg dest_mem))
      | OrMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> OrMM (size, s, d))
      | XorIM (size, src_imm, dest_mem) -> mk_single (XorIM (size, src_imm, resolve_reg dest_mem))
      | XorMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> XorMM (size, s, d))
      | ShlI (size, src_imm, dest_mem) -> mk_single (ShlI (size, src_imm, resolve_reg dest_mem))
      | ShlR (size, dest_mem) -> mk_single (ShlR (size, resolve_reg dest_mem))
      | ShrI (size, src_imm, dest_mem) -> mk_single (ShrI (size, src_imm, resolve_reg dest_mem))
      | ShrR (size, dest_mem) -> mk_single (ShrR (size, resolve_reg dest_mem))
      | SarI (size, src_imm, dest_mem) -> mk_single (SarI (size, src_imm, resolve_reg dest_mem))
      | SarR (size, dest_mem) -> mk_single (SarR (size, resolve_reg dest_mem))
      | CmpMI (size, mem, imm) -> mk_single (CmpMI (size, resolve_reg mem, imm))
      | CmpMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> CmpMM (size, s, d))
      | TestMR (size, arg1, arg2) ->
        let arg1 = resolve_reg arg1 in
        let arg2 = resolve_reg arg2 in
        if VReg.is_reg_value arg2 then
          mk_single (TestMR (size, arg1, arg2))
        else if VReg.is_reg_value arg1 then
          (* If register was resolved to memory location but memory was resolved to register, swap *)
          mk_single (TestMR (size, arg2, arg1))
        else
          (* If both arguments are memory location, create and move to new vreg for second argument *)
          let new_vreg = mk_vreg () in
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, arg2, new_vreg));
          this#add_instr (instr_id, TestMR (size, arg1, new_vreg))
      | SetCC (cc, mem) -> mk_single (SetCC (cc, resolve_reg mem))
      | CallM (size, mem) -> mk_single (CallM (size, resolve_reg mem))
      | PushI _
      | ConvertDouble _
      | Jmp _
      | JmpCC _
      | CallL _
      | Leave
      | Ret
      | Syscall ->
        mk_single instr

    (* Resolve a register, replacing the register with a memory if it has been resolved to a
       stack slot. *)
    method resolve_reg ~block vreg =
      let vreg = VReg.get_vreg_alias vreg in
      (match vreg.resolution with
      | MemoryAddress mem ->
        vreg.resolution <- MemoryAddress (this#force_registers_in_address ~block mem)
      | _ -> ());
      vreg

    (* The base and offset in memory addresses must be a register. If the base or offset vreg has
       been resolve to a stack slot, emit a mov to copy this stack slot to a register and use the
       register in the memory address instead. *)
    method force_registers_in_address ~block addr =
      let { MemoryAddress.offset; base; index_and_scale } = addr in
      let base' =
        match base with
        | RegBase reg ->
          let mem_vreg = this#resolve_reg ~block reg in
          if VReg.is_memory_value mem_vreg then (
            let vreg = this#mk_vreg () in
            this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Size64, mem_vreg, vreg));
            MemoryAddress.RegBase vreg
          ) else
            base
        | _ -> base
      in
      let index_and_scale' =
        match index_and_scale with
        | Some (reg, scale) ->
          let mem_vreg = this#resolve_reg ~block reg in
          if VReg.is_memory_value mem_vreg then (
            let vreg = this#mk_vreg () in
            this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Size64, mem_vreg, vreg));
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
