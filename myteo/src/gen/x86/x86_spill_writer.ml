open X86_gen_context
open X86_instructions

class spill_writer ~gcx =
  object (this)
    val mutable new_vregs = VRegSet.empty

    val mutable current_instruction_builder = []

    val mutable current_block_builders = []

    method new_vregs = new_vregs

    method mk_vreg ~block =
      let vreg = VReg.mk ~resolution:Unresolved ~func:(Some block.Block.func) in
      new_vregs <- VRegSet.add vreg new_vregs;
      vreg

    method add_instr instr = current_instruction_builder <- instr :: current_instruction_builder

    method write_block_spills (block : virtual_block) =
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
      let mk_vreg () = this#mk_vreg ~block in
      let resolve_reg reg = this#resolve_reg ~block reg in
      let resolve_mem mem = this#resolve_mem ~block mem in
      let resolve_binop_single_mem size src_mem dest_mem f =
        let src_mem = resolve_mem src_mem in
        let dest_mem = resolve_mem dest_mem in
        match (src_mem, dest_mem) with
        | (Mem src_mem, Mem dest_mem) ->
          let src_vreg = mk_vreg () in
          this#add_instr
            (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, Mem src_mem, Reg src_vreg));
          this#add_instr (instr_id, f (Reg src_vreg) (Mem dest_mem))
        | _ -> mk_single (f src_mem dest_mem)
      in
      (* Must have a register not a memory address - if necessary create new register then move *)
      let force_register_write size reg f =
        match resolve_reg reg with
        | VirtualRegister.StackSlot mem ->
          let vreg_dest = mk_vreg () in
          this#add_instr (instr_id, f vreg_dest);
          this#add_instr (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, Reg vreg_dest, Mem mem))
        | _ -> mk_single (f reg)
      in
      match instr with
      | PushM mem -> mk_single (PushM (resolve_mem mem))
      | PopM mem -> mk_single (PopM (resolve_mem mem))
      | MovIM (size, src_imm, dest_mem) -> mk_single (MovIM (size, src_imm, resolve_mem dest_mem))
      | MovMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> MovMM (size, s, d))
      | Lea (size, addr, reg) -> force_register_write size reg (fun reg' -> Lea (size, addr, reg'))
      | NegM (size, mem) -> mk_single (NegM (size, resolve_mem mem))
      | AddIM (size, src_imm, dest_mem) -> mk_single (AddIM (size, src_imm, resolve_mem dest_mem))
      | AddMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> AddMM (size, s, d))
      | SubIM (size, src_imm, dest_mem) -> mk_single (SubIM (size, src_imm, resolve_mem dest_mem))
      | SubMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> SubMM (size, s, d))
      | IMulMR (size, src_mem, dest_reg) -> mk_single (IMulMR (size, resolve_mem src_mem, dest_reg))
      | IMulMIR (size, src_mem, src_imm, dest_reg) ->
        force_register_write (size_of_immediate src_imm) dest_reg (fun dest_reg' ->
            IMulMIR (size, resolve_mem src_mem, src_imm, dest_reg'))
      | IDiv (size, mem) -> mk_single (IDiv (size, resolve_mem mem))
      | NotM (size, mem) -> mk_single (NotM (size, resolve_mem mem))
      | AndIM (size, src_imm, dest_mem) -> mk_single (AndIM (size, src_imm, resolve_mem dest_mem))
      | AndMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> AndMM (size, s, d))
      | OrIM (size, src_imm, dest_mem) -> mk_single (OrIM (size, src_imm, resolve_mem dest_mem))
      | OrMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> OrMM (size, s, d))
      | XorIM (size, src_imm, dest_mem) -> mk_single (XorIM (size, src_imm, resolve_mem dest_mem))
      | XorMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> XorMM (size, s, d))
      | ShlI (size, src_imm, dest_mem) -> mk_single (ShlI (size, src_imm, resolve_mem dest_mem))
      | ShlR (size, dest_mem) -> mk_single (ShlR (size, resolve_mem dest_mem))
      | ShrI (size, src_imm, dest_mem) -> mk_single (ShrI (size, src_imm, resolve_mem dest_mem))
      | ShrR (size, dest_mem) -> mk_single (ShrR (size, resolve_mem dest_mem))
      | SarI (size, src_imm, dest_mem) -> mk_single (SarI (size, src_imm, resolve_mem dest_mem))
      | SarR (size, dest_mem) -> mk_single (SarR (size, resolve_mem dest_mem))
      | CmpMI (size, mem, imm) -> mk_single (CmpMI (size, resolve_mem mem, imm))
      | CmpMM (size, src_mem, dest_mem) ->
        resolve_binop_single_mem size src_mem dest_mem (fun s d -> CmpMM (size, s, d))
      | TestMR (size, mem, reg) ->
        (match resolve_reg reg with
        | StackSlot reg_resolve_mem ->
          (* If register was resolved to memory location but memory was resolved to register, swap *)
          (match resolve_mem mem with
          | Reg mem_resolve_reg -> mk_single (TestMR (size, Mem reg_resolve_mem, mem_resolve_reg))
          (* If both arguments are memory location, create and move to new vreg for second argument *)
          | Mem mem_resolve_mem ->
            let new_vreg = mk_vreg () in
            this#add_instr
              (Gcx.mk_instr_id_for_block ~gcx block, MovMM (size, Mem reg_resolve_mem, Reg new_vreg));
            this#add_instr (instr_id, TestMR (size, Mem mem_resolve_mem, new_vreg)))
        | _ -> mk_single (TestMR (size, resolve_mem mem, reg)))
      | SetCC (cc, mem) -> mk_single (SetCC (cc, resolve_mem mem))
      | CallM (size, mem) -> mk_single (CallM (size, resolve_mem mem))
      | PushI _
      | Jmp _
      | JmpCC _
      | CallL _
      | Leave
      | Ret
      | Syscall ->
        mk_single instr

    (* Resolve a register, replacing the register with a memory if it has been resolved to a
       stack slot. *)
    method resolve_reg ~block reg =
      let open VirtualRegister in
      match VReg.get_vreg_resolution reg with
      | StackSlot mem -> StackSlot (this#force_registers_in_address ~block mem)
      | other -> other

    (* Resolve a register or memory address, replacing the register with a memory if it has been
       resolved to a stack slot. *)
    method resolve_mem ~block mem =
      let open Instruction in
      match mem with
      | Reg vreg ->
        (match VReg.get_vreg_resolution vreg with
        | StackSlot mem -> Mem (this#force_registers_in_address ~block mem)
        | _ -> mem)
      | Mem mem -> Mem (this#force_registers_in_address ~block mem)

    (* The base and offset in memory addresses must be a register. If the base or offset vreg has
       been resolve to a stack slot, emit a mov to copy this stack slot to a register and use the
       register in the memory address instead. *)
    method force_registers_in_address ~block addr =
      match addr with
      | PhysicalAddress { offset; base; index_and_scale } ->
        let base' =
          match base with
          | RegBase reg ->
            (match this#resolve_reg ~block reg with
            | StackSlot mem ->
              let vreg = this#mk_vreg ~block in
              this#add_instr
                (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Size64, Mem mem, Reg vreg));
              RegBase vreg
            | _ -> base)
          | _ -> base
        in
        let index_and_scale' =
          match index_and_scale with
          | Some (reg, scale) ->
            (match this#resolve_reg ~block reg with
            | StackSlot mem ->
              let vreg = this#mk_vreg ~block in
              this#add_instr
                (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Size64, Mem mem, Reg vreg));
              Some (vreg, scale)
            | _ -> index_and_scale)
          | _ -> index_and_scale
        in
        if base == base' && index_and_scale == index_and_scale' then
          addr
        else
          PhysicalAddress { offset; base = base'; index_and_scale = index_and_scale' }
      | _ -> addr
  end
