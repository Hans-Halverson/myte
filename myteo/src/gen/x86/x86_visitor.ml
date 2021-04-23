open X86_instructions

class instruction_visitor =
  object (this)
    method visit_instruction ~(block : virtual_block) (instr : virtual_instruction) =
      let open Instruction in
      let visit_read_vreg reg = this#visit_read_vreg ~block reg in
      let visit_read_mem mem = this#visit_read_mem ~block mem in
      let visit_write_mem mem = this#visit_write_mem ~block mem in
      let (_, instr) = instr in
      match instr with
      | PushM read_mem
      | CmpMI (read_mem, _) ->
        visit_read_mem read_mem
      | CallR read_vreg
      | IDivR read_vreg ->
        this#visit_read_vreg ~block read_vreg
      | PopM write_mem
      | MovIM (_, write_mem) ->
        visit_write_mem write_mem
      | NegR read_write_vreg
      | NotR read_write_vreg
      | AddIR (_, read_write_vreg)
      | SubIR (_, read_write_vreg)
      | AndIR (_, read_write_vreg)
      | OrIR (_, read_write_vreg) ->
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | CmpMM (read_mem1, read_mem2) ->
        visit_read_mem read_mem1;
        visit_read_mem read_mem2
      | MovMM (read_mem, write_mem) ->
        visit_read_mem read_mem;
        visit_write_mem write_mem
      | IMulRIR (read_vreg, _, write_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_write_vreg ~block write_vreg
      | XorMM (read_mem, read_write_mem) ->
        visit_read_mem read_mem;
        visit_read_mem read_write_mem;
        visit_write_mem read_write_mem
      | AddRR (read_vreg, read_write_vreg)
      | SubRR (read_vreg, read_write_vreg)
      | IMulRR (read_vreg, read_write_vreg)
      | AndRR (read_vreg, read_write_vreg)
      | OrRR (read_vreg, read_write_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | TestMR (read_mem, read_vreg) ->
        visit_read_mem read_mem;
        visit_read_vreg read_vreg
      | Lea (mem, write_vreg) ->
        this#visit_memory_address ~block mem;
        this#visit_write_vreg ~block write_vreg
      | SetCC (_, write_vreg) -> this#visit_write_vreg ~block write_vreg
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

    method visit_read_mem ~block mem =
      let open Instruction in
      match mem with
      | Reg reg -> this#visit_read_vreg ~block reg
      | Mem addr -> this#visit_memory_address ~block addr

    method visit_write_mem ~block mem =
      let open Instruction in
      match mem with
      | Reg reg -> this#visit_write_vreg ~block reg
      | Mem addr -> this#visit_memory_address ~block addr

    method visit_read_vreg ~block:_ _vreg = ()

    method visit_write_vreg ~block:_ _vreg = ()
  end
