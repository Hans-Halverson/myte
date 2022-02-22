open X86_64_instructions

class instruction_visitor =
  object (this)
    method visit_instruction ~(block : Block.t) (instr : Instruction.t) =
      let open Instruction in
      let visit_read_vreg vreg = this#visit_read_vreg ~block vreg in
      let visit_write_vreg vreg = this#visit_write_vreg ~block vreg in
      let (_, instr) = instr in
      match instr with
      | PushM read_vreg
      | CmpMI (_, read_vreg, _)
      | CallM (_, read_vreg)
      | IDiv (_, read_vreg) ->
        visit_read_vreg read_vreg
      | PopM write_vreg
      | MovIM (_, _, write_vreg)
      | SetCC (_, write_vreg) ->
        visit_write_vreg write_vreg
      | NegM (_, read_write_vreg)
      | NotM (_, read_write_vreg)
      | AddIM (_, _, read_write_vreg)
      | SubIM (_, _, read_write_vreg)
      | AndIM (_, _, read_write_vreg)
      | OrIM (_, _, read_write_vreg)
      | XorIM (_, _, read_write_vreg)
      | ShlI (_, _, read_write_vreg)
      | ShrI (_, _, read_write_vreg)
      | SarI (_, _, read_write_vreg)
      | ShlR (_, read_write_vreg)
      | ShrR (_, read_write_vreg)
      | SarR (_, read_write_vreg) ->
        visit_read_vreg read_write_vreg;
        visit_write_vreg read_write_vreg
      | CmpMM (_, read_vreg1, read_vreg2) ->
        visit_read_vreg read_vreg1;
        visit_read_vreg read_vreg2
      | MovMM (_, read_vreg, write_vreg) ->
        visit_read_vreg read_vreg;
        visit_write_vreg write_vreg
      | MovSX (_, _, read_vreg, write_vreg)
      | MovZX (_, _, read_vreg, write_vreg)
      | IMulMIR (_, read_vreg, _, write_vreg) ->
        visit_read_vreg read_vreg;
        visit_write_vreg write_vreg
      | AddMM (_, read_vreg, read_write_vreg)
      | SubMM (_, read_vreg, read_write_vreg)
      | AndMM (_, read_vreg, read_write_vreg)
      | OrMM (_, read_vreg, read_write_vreg)
      | XorMM (_, read_vreg, read_write_vreg) ->
        visit_read_vreg read_vreg;
        visit_read_vreg read_write_vreg;
        visit_write_vreg read_write_vreg
      | TestMR (_, read_vreg1, read_vreg2) ->
        visit_read_vreg read_vreg1;
        visit_read_vreg read_vreg2
      | IMulMR (_, read_vreg, read_write_vreg) ->
        visit_read_vreg read_vreg;
        visit_read_vreg read_write_vreg;
        visit_write_vreg read_write_vreg
      | Lea (_, addr, write_vreg) ->
        this#visit_memory_address ~block addr;
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
      (match memory_address.base with
      | NoBase -> ()
      | RegBase reg -> this#visit_read_vreg ~block reg
      | IPBase -> ());
      match memory_address.index_and_scale with
      | None -> ()
      | Some (index_vreg, _) -> this#visit_read_vreg ~block index_vreg

    method visit_read_vreg ~block vreg =
      match vreg.resolution with
      | MemoryAddress addr -> this#visit_memory_address ~block addr
      | _ -> ()

    method visit_write_vreg ~block vreg =
      match vreg.resolution with
      | MemoryAddress addr -> this#visit_memory_address ~block addr
      | _ -> ()
  end
