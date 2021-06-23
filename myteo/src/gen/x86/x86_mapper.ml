open X86_instructions

class instruction_mapper =
  object (this)
    method map_instruction (instr_with_id : virtual_instruction) =
      let open Instruction in
      let (instr_id, instr) = instr_with_id in
      let instr' =
        match instr with
        | PushI imm ->
          let imm' = this#map_imm imm in
          if imm == imm' then
            instr
          else
            PushI imm'
        | PushM mem ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            PushM mem'
        | PopM mem ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            PopM mem'
        | MovIM (imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            MovIM (imm', mem')
        | MovMM (size, src_mem, dest_mem) ->
          let src_mem' = this#map_mem src_mem in
          let dest_mem' = this#map_mem dest_mem in
          if src_mem == src_mem' && dest_mem == dest_mem' then
            instr
          else
            MovMM (size, src_mem', dest_mem')
        | Lea (size, addr, reg) ->
          let addr' = this#map_addr addr in
          let reg' = this#map_reg reg in
          if addr == addr' && reg == reg' then
            instr
          else
            Lea (size, addr', reg')
        | NegM (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            NegM (size, mem')
        | AddIM (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            AddIM (size, imm', mem')
        | AddMM (size, src_mem, dest_mem) ->
          let src_mem' = this#map_mem src_mem in
          let dest_mem' = this#map_mem dest_mem in
          if src_mem == src_mem' && dest_mem == dest_mem' then
            instr
          else
            AddMM (size, src_mem', dest_mem')
        | SubIM (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            SubIM (size, imm', mem')
        | SubMM (size, src_mem, dest_mem) ->
          let src_mem' = this#map_mem src_mem in
          let dest_mem' = this#map_mem dest_mem in
          if src_mem == src_mem' && dest_mem == dest_mem' then
            instr
          else
            SubMM (size, src_mem', dest_mem')
        | IMulMR (size, mem, reg) ->
          let mem' = this#map_mem mem in
          let reg' = this#map_reg reg in
          if mem == mem' && reg == reg' then
            instr
          else
            IMulMR (size, mem', reg')
        | IMulMIR (size, mem, imm, reg) ->
          let mem' = this#map_mem mem in
          let imm' = this#map_imm imm in
          let reg' = this#map_reg reg in
          if mem == mem' && imm == imm' && reg == reg' then
            instr
          else
            IMulMIR (size, mem', imm', reg')
        | IDiv (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            IDiv (size, mem')
        | NotM (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            NotM (size, mem')
        | AndIM (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            AndIM (size, imm', mem')
        | AndMM (size, src_mem, dest_mem) ->
          let src_mem' = this#map_mem src_mem in
          let dest_mem' = this#map_mem dest_mem in
          if src_mem == src_mem' && dest_mem == dest_mem' then
            instr
          else
            AndMM (size, src_mem', dest_mem')
        | OrIM (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            OrIM (size, imm', mem')
        | OrMM (size, src_mem, dest_mem) ->
          let src_mem' = this#map_mem src_mem in
          let dest_mem' = this#map_mem dest_mem in
          if src_mem == src_mem' && dest_mem == dest_mem' then
            instr
          else
            OrMM (size, src_mem', dest_mem')
        | XorIM (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            XorIM (size, imm', mem')
        | XorMM (size, src_mem, dest_mem) ->
          let src_mem' = this#map_mem src_mem in
          let dest_mem' = this#map_mem dest_mem in
          if src_mem == src_mem' && dest_mem == dest_mem' then
            instr
          else
            XorMM (size, src_mem', dest_mem')
        | ShlI (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            ShlI (size, imm', mem')
        | ShlR (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            ShlR (size, mem')
        | ShrI (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            ShrI (size, imm', mem')
        | ShrR (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            ShrR (size, mem')
        | SarI (size, imm, mem) ->
          let imm' = this#map_imm imm in
          let mem' = this#map_mem mem in
          if imm == imm' && mem == mem' then
            instr
          else
            SarI (size, imm', mem')
        | SarR (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            SarR (size, mem')
        | CmpMI (size, mem, imm) ->
          let mem' = this#map_mem mem in
          let imm' = this#map_imm imm in
          if mem == mem' && imm == imm' then
            instr
          else
            CmpMI (size, mem', imm')
        | CmpMM (size, left_mem, right_mem) ->
          let left_mem' = this#map_mem left_mem in
          let right_mem' = this#map_mem right_mem in
          if left_mem == left_mem' && right_mem == right_mem' then
            instr
          else
            CmpMM (size, left_mem', right_mem')
        | TestMR (size, mem, reg) ->
          let mem' = this#map_mem mem in
          let reg' = this#map_reg reg in
          if mem == mem' && reg == reg' then
            instr
          else
            TestMR (size, mem', reg')
        | SetCC (cc, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            SetCC (cc, mem')
        | CallM (size, mem) ->
          let mem' = this#map_mem mem in
          if mem == mem' then
            instr
          else
            CallM (size, mem')
        | CallL _
        | Jmp _
        | JmpCC _
        | Leave
        | Ret
        | Syscall ->
          instr
      in
      if instr == instr' then
        instr_with_id
      else
        (instr_id, instr')

    method map_imm imm = imm

    method map_reg reg = reg

    method map_mem mem = mem

    method map_addr addr = addr
  end
