open Basic_collections
open X86_64_gen_context
open X86_64_instructions

class use_def_finder color_to_representative_operand =
  object (this)
    inherit X86_64_visitor.instruction_visitor as super

    method add_register_use ~block:_ _reg = ()

    method add_register_def ~block:_ _reg = ()

    method get_representative_register reg = RegMap.find reg color_to_representative_operand

    method! visit_instruction ~block instr_with_id =
      let open Instruction in
      let (_, instr) = instr_with_id in
      match instr with
      (* Calls define all caller save registers *)
      | CallM _
      | CallL _ ->
        RegSet.iter
          (fun reg ->
            let color_rep_reg = this#get_representative_register reg in
            this#add_register_def ~block color_rep_reg)
          caller_saved_registers;
        super#visit_instruction ~block instr_with_id
      (* IDiv uses the value in register A and writes to registers A and D *)
      | IDiv _ ->
        let reg_a = this#get_representative_register A in
        let reg_d = this#get_representative_register D in
        this#add_register_use ~block reg_a;
        this#add_register_def ~block reg_a;
        this#add_register_def ~block reg_d;
        super#visit_instruction ~block instr_with_id
      (* ConvertDouble (e.g. cdq) uses the value in register A and writes to register D *)
      | ConvertDouble _ ->
        this#add_register_use ~block (this#get_representative_register A);
        this#add_register_def ~block (this#get_representative_register D);
        super#visit_instruction ~block instr_with_id
      (* Shifts with register shift argument implicitly use value in register C *)
      | ShlR _
      | ShrR _
      | SarR _ ->
        this#add_register_use ~block (this#get_representative_register C);
        super#visit_instruction ~block instr_with_id
      (* Xor of a register with itself zeros the register, and only counts as a def, not a use, as
         the result is completely independent of the original value in the register. *)
      | XorMM (_, reg1, reg2) when Operand.is_reg_value reg1 && reg1.id = reg2.id ->
        this#add_register_def ~block reg1
      | _ -> super#visit_instruction ~block instr_with_id

    method! visit_read_operand ~block op =
      if Operand.is_reg_value op then
        this#add_register_use ~block op
      else
        super#visit_read_operand ~block op

    method! visit_write_operand ~block op =
      if Operand.is_reg_value op then
        this#add_register_def ~block op
      else
        super#visit_write_operand ~block op
  end

class analyze_regs_init_visitor (blocks : Block.t List.t) color_to_operand =
  object (this)
    inherit use_def_finder color_to_operand

    val mutable prev_blocks =
      List.fold_left (fun acc block -> IMap.add block.Block.id ISet.empty acc) IMap.empty blocks

    val mutable reg_use_blocks = OperandMap.empty

    val mutable reg_def_blocks = OperandMap.empty

    val mutable reg_use_before_def_blocks = OperandMap.empty

    method prev_blocks = prev_blocks

    method reg_use_blocks = reg_use_blocks

    method reg_def_blocks = reg_def_blocks

    method reg_use_before_def_blocks = reg_use_before_def_blocks

    method run () = List.iter (fun block -> this#visit_block block) blocks

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method! visit_block_edge ~block next_block_id =
      prev_blocks <-
        IMap.add next_block_id (ISet.add block.id (IMap.find next_block_id prev_blocks)) prev_blocks

    method! add_register_use ~(block : Block.t) reg =
      reg_use_blocks <- OIMMap.add reg block.id reg_use_blocks

    method! add_register_def ~(block : Block.t) reg =
      if
        OIMMap.contains reg block.id reg_use_blocks
        && not (OIMMap.contains reg block.id reg_def_blocks)
      then
        reg_use_before_def_blocks <- OIMMap.add reg block.id reg_use_before_def_blocks;
      reg_def_blocks <- OIMMap.add reg block.id reg_def_blocks
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
  let live_in = ref IMap.empty in
  let live_out = ref IMap.empty in
  List.iter
    (fun block ->
      let open Block in
      live_in := IMap.add block.id [] !live_in;
      live_out := IMap.add block.id [] !live_out)
    blocks;

  (* Propagate a single register backwards through the program, building liveness sets as we go *)
  let set_contains set block_id reg =
    match IMap.find block_id !set with
    | hd :: _ when hd == reg -> true
    | _ -> false
  in
  let set_add set block_id reg = set := IMap.add block_id (reg :: IMap.find block_id !set) !set in
  let rec propagate_backwards ~block_id ~reg =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       reg is defined (unless the reg is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block_id reg))
      && ( (not (OIMMap.contains reg block_id reg_def_blocks))
         || OIMMap.contains reg block_id reg_use_before_def_blocks )
    then (
      set_add live_in block_id reg;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block reg) then set_add live_out prev_block reg;
          propagate_backwards ~block_id:prev_block ~reg)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  OperandMap.iter
    (fun reg use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~reg) use_blocks)
    reg_use_blocks;

  (!live_in, !live_out)

class analyze_virtual_stack_slots_init_visitor ~(gcx : Gcx.t) =
  object (this)
    inherit X86_64_visitor.instruction_visitor as super

    val mutable prev_blocks =
      IMap.fold (fun block_id _ acc -> IMap.add block_id ISet.empty acc) gcx.blocks_by_id IMap.empty

    val mutable vslot_use_blocks = OperandMap.empty

    val mutable vslot_def_blocks = OperandMap.empty

    val mutable vslot_use_before_def_blocks = OperandMap.empty

    method prev_blocks = prev_blocks

    method vslot_use_blocks = vslot_use_blocks

    method vslot_def_blocks = vslot_def_blocks

    method vslot_use_before_def_blocks = vslot_use_before_def_blocks

    method run () = IMap.iter (fun _ block -> this#visit_block block) gcx.blocks_by_id

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method! visit_block_edge ~block next_block_id =
      prev_blocks <-
        IMap.add next_block_id (ISet.add block.id (IMap.find next_block_id prev_blocks)) prev_blocks

    method! visit_read_operand ~block op =
      match op.value with
      | VirtualStackSlot -> vslot_use_blocks <- OIMMap.add op block.id vslot_use_blocks
      | _ -> super#visit_read_operand ~block op

    method! visit_write_operand ~block op =
      match op.value with
      | VirtualStackSlot ->
        if
          OIMMap.contains op block.id vslot_use_blocks
          && not (OIMMap.contains op block.id vslot_def_blocks)
        then
          vslot_use_before_def_blocks <- OIMMap.add op block.id vslot_use_before_def_blocks;
        vslot_def_blocks <- OIMMap.add op block.id vslot_def_blocks
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
  let live_in = ref IMap.empty in
  let live_out = ref IMap.empty in
  IMap.iter
    (fun block_id _ ->
      live_in := IMap.add block_id [] !live_in;
      live_out := IMap.add block_id [] !live_out)
    gcx.blocks_by_id;

  (* Propagate a single variable backwards through the program, building liveness sets as we go *)
  let set_contains set block_id vslot =
    match IMap.find block_id !set with
    | hd :: _ when hd == vslot -> true
    | _ -> false
  in
  let set_add set block_id vslot =
    set := IMap.add block_id (vslot :: IMap.find block_id !set) !set
  in
  let rec propagate_backwards ~block_id ~vslot =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       var is defined (unless the var is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block_id vslot))
      && ( (not (OIMMap.contains vslot block_id vslot_def_blocks))
         || OIMMap.contains vslot block_id vslot_use_before_def_blocks )
    then (
      set_add live_in block_id vslot;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block vslot) then set_add live_out prev_block vslot;
          propagate_backwards ~block_id:prev_block ~vslot)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  OperandMap.iter
    (fun vslot use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~vslot) use_blocks)
    vslot_use_blocks;

  (!live_in, !live_out)
