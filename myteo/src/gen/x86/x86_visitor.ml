open X86_instructions

class instruction_visitor =
  object (this)
    method visit_instruction ~(block : virtual_block) (instr : virtual_instruction) =
      let open Instruction in
      let (_, instr) = instr in
      match instr with
      | PushR read_vreg
      | CallR read_vreg
      | IDivR read_vreg
      | CmpRI (read_vreg, _) ->
        this#visit_read_vreg ~block read_vreg
      | PopR write_vreg -> this#visit_write_vreg ~block write_vreg
      | NegR read_write_vreg
      | NotR read_write_vreg
      | AddIR (_, read_write_vreg)
      | SubIR (_, read_write_vreg)
      | AndIR (_, read_write_vreg)
      | OrIR (_, read_write_vreg) ->
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | PushM mem
      | CmpMI (mem, _) ->
        this#visit_memory_address ~block mem
      | CmpRR (read_vreg1, read_vreg2)
      | TestRR (read_vreg1, read_vreg2) ->
        this#visit_read_vreg ~block read_vreg1;
        this#visit_read_vreg ~block read_vreg2
      | MovRR (read_vreg, write_vreg)
      | IMulRIR (read_vreg, _, write_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_write_vreg ~block write_vreg
      | AddRR (read_vreg, read_write_vreg)
      | SubRR (read_vreg, read_write_vreg)
      | IMulRR (read_vreg, read_write_vreg)
      | AndRR (read_vreg, read_write_vreg)
      | OrRR (read_vreg, read_write_vreg)
      | XorRR (read_vreg, read_write_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | MovMR (mem, write_vreg)
      | Lea (mem, write_vreg) ->
        this#visit_memory_address ~block mem;
        this#visit_write_vreg ~block write_vreg
      | MovRM (read_vreg, mem)
      | CmpRM (read_vreg, mem)
      | CmpMR (mem, read_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_memory_address ~block mem
      | MovIR (_, write_vreg)
      | SetCC (_, write_vreg) ->
        this#visit_write_vreg ~block write_vreg
      | MovIM (_, mem) -> this#visit_memory_address ~block mem
      | Jmp next_block_id
      | JmpCC (_, next_block_id) ->
        this#visit_block_edge ~block next_block_id
      | PushI _
      | CallL _
      | Leave
      | Ret
      | Syscall ->
        ()

    method visit_block_edge ~block:_ _next_block_id = ()

    method visit_memory_address ~block memory_address =
      match memory_address with
      | VirtualStackSlot _ -> ()
      | PhysicalAddress mem ->
        Option.iter (this#visit_read_vreg ~block) mem.base;
        (match mem.index_and_scale with
        | None -> ()
        | Some (index_vreg, _) -> this#visit_read_vreg ~block index_vreg)

    method visit_read_vreg ~block:_ _vreg_id = ()

    method visit_write_vreg ~block:_ _vreg_id = ()
  end
