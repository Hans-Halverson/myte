open X86_gen_context
open X86_instructions

class spill_writer ~gcx =
  object (this)
    val mutable new_vregs = VRegSet.empty

    method new_vregs = new_vregs

    method mk_vreg ~block =
      let vreg = VReg.mk ~resolution:Unresolved ~func:(Some block.Block.func) in
      new_vregs <- VRegSet.add vreg new_vregs;
      vreg

    method write_block_spills (block : virtual_block) =
      block.instructions <-
        List.map (this#map_instruction ~block) block.instructions |> List.flatten

    (* Every memory location (M suffix) must be visited in case it is a Reg that should become a Mem.
       Every MM instruction must make sure that it does not become filled with two memory locations.
       Enforce locations that need a register (R suffix) and ensure they do not become memory locations.

       TODO: Enforce that all vregs in memory locations need to be a register and R suffixi instrs *)
    method map_instruction ~block instr =
      let open Instruction in
      let (instr_id, instr) = instr in
      let mk_single instr = [(instr_id, instr)] in
      let mk_vreg () = this#mk_vreg ~block in
      let resolve_binop_single_mem src_mem dest_mem f =
        let src_mem = this#resolve_mem src_mem in
        let dest_mem = this#resolve_mem dest_mem in
        match (src_mem, dest_mem) with
        | (Mem src_mem, Mem dest_mem) ->
          let src_vreg = mk_vreg () in
          [
            (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Mem src_mem, Reg src_vreg));
            (instr_id, f (Reg src_vreg) (Mem dest_mem));
          ]
        | _ -> mk_single (f src_mem dest_mem)
      in
      (* Must have a register not a memory address - if necessary create new register then move *)
      let force_register_write reg f =
        match Gcx.get_vreg_resolution ~gcx reg with
        | StackSlot mem ->
          let vreg_dest = mk_vreg () in
          [
            (instr_id, f vreg_dest);
            (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Reg vreg_dest, Mem mem));
          ]
        | _ -> mk_single (f reg)
      in
      match instr with
      | PushM mem -> mk_single (PushM (this#resolve_mem mem))
      | PopM mem -> mk_single (PopM (this#resolve_mem mem))
      | MovIM (src_imm, dest_mem) -> mk_single (MovIM (src_imm, this#resolve_mem dest_mem))
      | MovMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> MovMM (s, d))
      | Lea (addr, reg) -> force_register_write reg (fun reg' -> Lea (addr, reg'))
      | NegM mem -> mk_single (NegM (this#resolve_mem mem))
      | AddIM (src_imm, dest_mem) -> mk_single (AddIM (src_imm, this#resolve_mem dest_mem))
      | AddMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> AddMM (s, d))
      | SubIM (src_imm, dest_mem) -> mk_single (SubIM (src_imm, this#resolve_mem dest_mem))
      | SubMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> SubMM (s, d))
      | IMulMR (src_mem, dest_reg) -> mk_single (IMulMR (this#resolve_mem src_mem, dest_reg))
      | IMulMIR (src_mem, src_imm, dest_reg) ->
        force_register_write dest_reg (fun dest_reg' ->
            IMulMIR (this#resolve_mem src_mem, src_imm, dest_reg'))
      | IDiv mem -> mk_single (IDiv (this#resolve_mem mem))
      | NotM mem -> mk_single (NotM (this#resolve_mem mem))
      | AndIM (src_imm, dest_mem) -> mk_single (AndIM (src_imm, this#resolve_mem dest_mem))
      | AndMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> AndMM (s, d))
      | OrIM (src_imm, dest_mem) -> mk_single (OrIM (src_imm, this#resolve_mem dest_mem))
      | OrMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> OrMM (s, d))
      | XorMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> XorMM (s, d))
      | CmpMI (mem, imm) -> mk_single (CmpMI (this#resolve_mem mem, imm))
      | CmpMM (src_mem, dest_mem) ->
        resolve_binop_single_mem src_mem dest_mem (fun s d -> CmpMM (s, d))
      | TestMR (mem, reg) ->
        (match Gcx.get_vreg_resolution ~gcx reg with
        | StackSlot reg_resolve_mem ->
          (* If register was resolved to memory location but memory was resolved to register, swap *)
          (match this#resolve_mem mem with
          | Reg mem_resolve_reg -> mk_single (TestMR (Mem reg_resolve_mem, mem_resolve_reg))
          (* If both arguments are memory location, create and move to new vreg for second argument *)
          | Mem mem_resolve_mem ->
            let new_vreg = mk_vreg () in
            [
              (Gcx.mk_instr_id_for_block ~gcx block, MovMM (Mem reg_resolve_mem, Reg new_vreg));
              (instr_id, TestMR (Mem mem_resolve_mem, new_vreg));
            ])
        | _ -> mk_single (TestMR (this#resolve_mem mem, reg)))
      | SetCC (cc, mem) -> mk_single (SetCC (cc, this#resolve_mem mem))
      | CallM mem -> mk_single (CallM (this#resolve_mem mem))
      | PushI _
      | Jmp _
      | JmpCC _
      | CallL _
      | Leave
      | Ret
      | Syscall ->
        mk_single instr

    method resolve_mem mem =
      let open Instruction in
      match mem with
      | Reg vreg ->
        (match Gcx.get_vreg_resolution ~gcx vreg with
        | StackSlot mem -> Mem mem
        | _ -> mem)
      | _ -> mem
  end
