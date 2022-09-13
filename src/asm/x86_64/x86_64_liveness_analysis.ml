open X86_64_builders
open X86_64_gen_context
open X86_64_instructions
open X86_64_register

class use_def_finder color_to_representative_operand =
  object (this)
    inherit X86_64_visitor.instruction_visitor as super

    method add_register_use ~block:_ (_reg : Operand.t) = ()

    method add_register_def ~block:_ (_reg : Operand.t) = ()

    method get_representative_register reg = RegMap.find reg color_to_representative_operand

    method! visit_instruction ~block instr =
      let open Instruction in
      match instr.instr with
      (* Calls define all caller save registers *)
      | CallM _
      | CallL _ ->
        RegSet.iter
          (fun reg -> this#add_register_def ~block (this#get_representative_register reg))
          caller_saved_registers;
        super#visit_instruction ~block instr
      (* IDiv uses the value in register A and writes to registers A and D *)
      | IDiv _ ->
        let reg_a = this#get_representative_register A in
        let reg_d = this#get_representative_register D in
        this#add_register_use ~block reg_a;
        this#add_register_def ~block reg_a;
        this#add_register_def ~block reg_d;
        super#visit_instruction ~block instr
      (* ConvertDouble (e.g. cdq) uses the value in register A and writes to register D *)
      | ConvertDouble _ ->
        this#add_register_use ~block (this#get_representative_register A);
        this#add_register_def ~block (this#get_representative_register D);
        super#visit_instruction ~block instr
      (* Shifts with register shift argument implicitly use value in register C *)
      | ShlR _
      | ShrR _
      | SarR _ ->
        this#add_register_use ~block (this#get_representative_register C);
        super#visit_instruction ~block instr
      (* Xor of a register with itself zeros the register, and only counts as a def, not a use, as
         the result is completely independent of the original value in the register. *)
      | XorMM (_, reg1, reg2) when Operand.is_reg_value reg1 && reg1.id = reg2.id ->
        this#visit_write_operand ~block reg1
      | _ -> super#visit_instruction ~block instr

    method resolve_register op =
      match op.Operand.value with
      | PhysicalRegister reg -> Some (this#get_representative_register reg)
      | VirtualRegister -> Some op
      | _ -> None

    method! visit_read_operand ~block op =
      match this#resolve_register op with
      | Some reg -> this#add_register_use ~block reg
      | None -> super#visit_read_operand ~block op

    method! visit_write_operand ~block op =
      match this#resolve_register op with
      | Some reg -> this#add_register_def ~block reg
      | None -> super#visit_write_operand ~block op
  end

class analyze_regs_init_visitor (blocks : Block.t List.t) color_to_operand =
  object (this)
    inherit use_def_finder color_to_operand

    val mutable prev_blocks = BlockMMap.empty

    val mutable reg_use_blocks = OBMMap.empty

    val mutable reg_def_blocks = OBMMap.empty

    val mutable reg_use_before_def_blocks = OBMMap.empty

    method prev_blocks = prev_blocks

    method reg_use_blocks = reg_use_blocks

    method reg_def_blocks = reg_def_blocks

    method reg_use_before_def_blocks = reg_use_before_def_blocks

    method run () = List.iter (fun block -> this#visit_block block) blocks

    method visit_block block = iter_instructions block (this#visit_instruction ~block)

    method! visit_block_edge ~block next_block =
      prev_blocks <- BlockMMap.add next_block block prev_blocks

    method! add_register_use ~(block : Block.t) (reg : Operand.t) =
      reg_use_blocks <- OBMMap.add reg block reg_use_blocks

    method! add_register_def ~(block : Block.t) (reg : Operand.t) =
      if OBMMap.contains reg block reg_use_blocks && not (OBMMap.contains reg block reg_def_blocks)
      then
        reg_use_before_def_blocks <- OBMMap.add reg block reg_use_before_def_blocks;
      reg_def_blocks <- OBMMap.add reg block reg_def_blocks
  end

let analyze_regs blocks color_to_operand =
  (* Calculate use and def blocks for each virtual and physical register *)
  let init_visitor = new analyze_regs_init_visitor blocks color_to_operand in
  init_visitor#run ();

  let prev_blocks = init_visitor#prev_blocks in
  let reg_use_blocks = init_visitor#reg_use_blocks in
  let reg_def_blocks = init_visitor#reg_def_blocks in
  let reg_use_before_def_blocks = init_visitor#reg_use_before_def_blocks in

  (* Initialize liveness sets *)
  let live_in = ref BlockMap.empty in
  let live_out = ref BlockMap.empty in
  List.iter
    (fun block ->
      live_in := BlockMap.add block [] !live_in;
      live_out := BlockMap.add block [] !live_out)
    blocks;

  (* Propagate a single register backwards through the program, building liveness sets as we go *)
  let set_contains set block reg =
    match BlockMap.find block !set with
    | hd :: _ when hd == reg -> true
    | _ -> false
  in
  let set_add set block reg = set := BlockMap.add block (reg :: BlockMap.find block !set) !set in
  let rec propagate_backwards ~block ~reg =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       reg is defined (unless the reg is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block reg))
      && ((not (OBMMap.contains reg block reg_def_blocks))
         || OBMMap.contains reg block reg_use_before_def_blocks)
    then (
      set_add live_in block reg;
      let prev_blocks = BlockMMap.find_all block prev_blocks in
      BlockMMap.VSet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block reg) then set_add live_out prev_block reg;
          propagate_backwards ~block:prev_block ~reg)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  OBMMap.iter
    (fun reg use_blocks ->
      OBMMap.VSet.iter (fun block -> propagate_backwards ~block ~reg) use_blocks)
    reg_use_blocks;

  (!live_in, !live_out)

class analyze_virtual_stack_slots_init_visitor ~(gcx : Gcx.t) =
  object (this)
    inherit X86_64_visitor.instruction_visitor as super

    val mutable prev_blocks = BlockMMap.empty

    val mutable vslot_use_blocks = OBMMap.empty

    val mutable vslot_def_blocks = OBMMap.empty

    val mutable vslot_use_before_def_blocks = OBMMap.empty

    method prev_blocks = prev_blocks

    method vslot_use_blocks = vslot_use_blocks

    method vslot_def_blocks = vslot_def_blocks

    method vslot_use_before_def_blocks = vslot_use_before_def_blocks

    method run () = funcs_iter_blocks gcx.funcs (fun block -> this#visit_block block)

    method visit_block block = iter_instructions block (this#visit_instruction ~block)

    method! visit_block_edge ~block next_block =
      prev_blocks <- BlockMMap.add next_block block prev_blocks

    method! visit_read_operand ~block op =
      match op.value with
      | VirtualStackSlot -> vslot_use_blocks <- OBMMap.add op block vslot_use_blocks
      | _ -> super#visit_read_operand ~block op

    method! visit_write_operand ~block op =
      match op.value with
      | VirtualStackSlot ->
        if
          OBMMap.contains op block vslot_use_blocks
          && not (OBMMap.contains op block vslot_def_blocks)
        then
          vslot_use_before_def_blocks <- OBMMap.add op block vslot_use_before_def_blocks;
        vslot_def_blocks <- OBMMap.add op block vslot_def_blocks
      | _ -> super#visit_write_operand ~block op
  end

let analyze_virtual_stack_slots ~(gcx : Gcx.t) =
  (* Calculate use and def blocks for each virtual stack slot *)
  let init_visitor = new analyze_virtual_stack_slots_init_visitor ~gcx in
  init_visitor#run ();

  let prev_blocks = init_visitor#prev_blocks in
  let vslot_use_blocks = init_visitor#vslot_use_blocks in
  let vslot_def_blocks = init_visitor#vslot_def_blocks in
  let vslot_use_before_def_blocks = init_visitor#vslot_use_before_def_blocks in

  (* Initialize liveness sets *)
  let live_in = ref BlockMap.empty in
  let live_out = ref BlockMap.empty in
  funcs_iter_blocks gcx.funcs (fun block ->
      live_in := BlockMap.add block [] !live_in;
      live_out := BlockMap.add block [] !live_out);

  (* Propagate a single variable backwards through the program, building liveness sets as we go *)
  let set_contains set block vslot =
    match BlockMap.find block !set with
    | hd :: _ when hd == vslot -> true
    | _ -> false
  in
  let set_add set block vslot =
    set := BlockMap.add block (vslot :: BlockMap.find block !set) !set
  in
  let rec propagate_backwards ~block ~vslot =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       var is defined (unless the var is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block vslot))
      && ((not (OBMMap.contains vslot block vslot_def_blocks))
         || OBMMap.contains vslot block vslot_use_before_def_blocks)
    then (
      set_add live_in block vslot;
      let prev_blocks = BlockMMap.find_all block prev_blocks in
      BlockMMap.VSet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block vslot) then set_add live_out prev_block vslot;
          propagate_backwards ~block:prev_block ~vslot)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  OBMMap.iter
    (fun vslot use_blocks ->
      OBMMap.VSet.iter (fun block -> propagate_backwards ~block ~vslot) use_blocks)
    vslot_use_blocks;

  (!live_in, !live_out)
