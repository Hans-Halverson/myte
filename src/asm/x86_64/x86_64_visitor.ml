open X86_64_instructions

class instruction_visitor =
  object (this)
    method visit_instruction ~(block : virtual_block) (instr : virtual_instruction) =
      let open Instruction in
      let visit_read_vreg reg = this#visit_read_vreg ~block reg in
      let visit_write_vreg reg = this#visit_write_vreg ~block reg in
      let visit_read_mem mem = this#visit_read_mem ~block mem in
      let visit_write_mem mem = this#visit_write_mem ~block mem in
      let (_, instr) = instr in
      match instr with
      | PushM read_mem
      | CmpMI (_, read_mem, _)
      | CallM (_, read_mem)
      | IDiv (_, read_mem) ->
        visit_read_mem read_mem
      | PopM write_mem
      | MovIM (_, _, write_mem)
      | SetCC (_, write_mem) ->
        visit_write_mem write_mem
      | NegM (_, read_write_mem)
      | NotM (_, read_write_mem)
      | AddIM (_, _, read_write_mem)
      | SubIM (_, _, read_write_mem)
      | AndIM (_, _, read_write_mem)
      | OrIM (_, _, read_write_mem)
      | XorIM (_, _, read_write_mem)
      | ShlI (_, _, read_write_mem)
      | ShrI (_, _, read_write_mem)
      | SarI (_, _, read_write_mem)
      | ShlR (_, read_write_mem)
      | ShrR (_, read_write_mem)
      | SarR (_, read_write_mem) ->
        visit_read_mem read_write_mem;
        visit_write_mem read_write_mem
      | CmpMM (_, read_mem1, read_mem2) ->
        visit_read_mem read_mem1;
        visit_read_mem read_mem2
      | MovMM (_, read_mem, write_mem) ->
        visit_read_mem read_mem;
        visit_write_mem write_mem
      | MovSX (_, _, read_mem, write_vreg)
      | MovZX (_, _, read_mem, write_vreg)
      | IMulMIR (_, read_mem, _, write_vreg) ->
        visit_read_mem read_mem;
        visit_write_vreg write_vreg
      | AddMM (_, read_mem, read_write_mem)
      | SubMM (_, read_mem, read_write_mem)
      | AndMM (_, read_mem, read_write_mem)
      | OrMM (_, read_mem, read_write_mem)
      | XorMM (_, read_mem, read_write_mem) ->
        visit_read_mem read_mem;
        visit_read_mem read_write_mem;
        visit_write_mem read_write_mem
      | TestMR (_, read_mem, read_vreg) ->
        visit_read_mem read_mem;
        visit_read_vreg read_vreg
      | IMulMR (_, read_mem, read_write_vreg) ->
        visit_read_mem read_mem;
        visit_read_vreg read_write_vreg;
        visit_write_vreg read_write_vreg
      | Lea (_, mem, write_vreg) ->
        this#visit_memory_address ~block mem;
        visit_write_vreg write_vreg
      | Jmp next_block_id
      | JmpCC (_, next_block_id) ->
        this#visit_block_edge ~block next_block_id
      | PushI _
      | CallL _
      | ConvertDouble _
      | Leave
      | Ret
      | Syscall ->
        ()

    method visit_block_edge ~block:_ _next_block_id = ()

    method visit_memory_address ~block memory_address =
      match memory_address with
      | VirtualStackSlot _ -> ()
      | FunctionStackArgument _ -> ()
      | PhysicalAddress mem ->
        (match mem.base with
        | NoBase -> ()
        | RegBase reg -> this#visit_read_vreg ~block reg
        | IPBase -> ());
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
