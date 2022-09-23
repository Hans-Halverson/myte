open X86_64_instructions

class instruction_visitor =
  object (this)
    method visit_instruction ~(block : Block.t) (instr : Instruction.t) =
      let open Instruction in
      let visit_read_operand op = this#visit_read_operand ~block op in
      let visit_write_operand op = this#visit_write_operand ~block op in
      match instr.instr with
      | PushM read_op
      | CmpMI (_, read_op, _)
      | CallM (_, read_op, _)
      | IDiv (_, read_op) ->
        visit_read_operand read_op
      | PopM write_op
      | MovIM (_, _, write_op)
      | SetCC (_, write_op) ->
        visit_write_operand write_op
      | NegM (_, read_write_op)
      | NotM (_, read_write_op)
      | AddIM (_, _, read_write_op)
      | SubIM (_, _, read_write_op)
      | AndIM (_, _, read_write_op)
      | OrIM (_, _, read_write_op)
      | XorIM (_, _, read_write_op)
      | ShlI (_, _, read_write_op)
      | ShrI (_, _, read_write_op)
      | SarI (_, _, read_write_op)
      | ShlR (_, read_write_op)
      | ShrR (_, read_write_op)
      | SarR (_, read_write_op) ->
        visit_read_operand read_write_op;
        visit_write_operand read_write_op
      | CmpMM (_, read_op1, read_op2) ->
        visit_read_operand read_op1;
        visit_read_operand read_op2
      | MovMM (_, read_op, write_op) ->
        visit_read_operand read_op;
        visit_write_operand write_op
      | MovSX (_, _, read_op, write_op)
      | MovZX (_, _, read_op, write_op)
      | ConvertIntToFloat (_, read_op, write_op)
      | ConvertFloatToInt (_, read_op, write_op)
      | IMulMIR (_, read_op, _, write_op) ->
        visit_read_operand read_op;
        visit_write_operand write_op
      | AddMM (_, read_op, read_write_op)
      | SubMM (_, read_op, read_write_op)
      | AndMM (_, read_op, read_write_op)
      | OrMM (_, read_op, read_write_op)
      | XorMM (_, read_op, read_write_op) ->
        visit_read_operand read_op;
        visit_read_operand read_write_op;
        visit_write_operand read_write_op
      | TestMR (_, read_op1, read_op2) ->
        visit_read_operand read_op1;
        visit_read_operand read_op2
      | MulMR (_, read_op, read_write_op)
      | FDivMR (_, read_op, read_write_op) ->
        visit_read_operand read_op;
        visit_read_operand read_write_op;
        visit_write_operand read_write_op
      | Lea (_, addr, write_op) ->
        this#visit_memory_address ~block addr;
        visit_write_operand write_op
      | Jmp next_block
      | JmpCC (_, next_block) ->
        this#visit_block_edge ~block (cast_to_block next_block)
      | PushI _
      | CallL _
      | ConvertDouble _
      | Leave
      | Ret
      | Syscall ->
        ()

    method visit_block_edge ~block:_ _next_block = ()

    method visit_memory_address ~block op =
      let addr = cast_to_memory_address op in
      (match addr.base with
      | NoBase -> ()
      | RegBase reg -> this#visit_read_operand ~block reg
      | IPBase -> ());
      match addr.index_and_scale with
      | None -> ()
      | Some (index_op, _) -> this#visit_read_operand ~block index_op

    method visit_read_operand ~block op =
      match op.value with
      | MemoryAddress _ -> this#visit_memory_address ~block op
      | _ -> ()

    method visit_write_operand ~block op =
      match op.value with
      | MemoryAddress _ -> this#visit_memory_address ~block op
      | _ -> ()
  end
