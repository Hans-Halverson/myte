open Basic_collections
open X86_gen_context
open X86_instructions

class analyze_vregs_init_visitor (blocks : virtual_block List.t) color_to_vreg =
  object (this)
    inherit X86_visitor.instruction_visitor as super

    val mutable prev_blocks =
      List.fold_left (fun acc block -> IMap.add block.Block.id ISet.empty acc) IMap.empty blocks

    val mutable vreg_use_blocks = VRegMap.empty

    val mutable vreg_def_blocks = VRegMap.empty

    val mutable vreg_use_before_def_blocks = VRegMap.empty

    method prev_blocks = prev_blocks

    method vreg_use_blocks = vreg_use_blocks

    method vreg_def_blocks = vreg_def_blocks

    method vreg_use_before_def_blocks = vreg_use_before_def_blocks

    method run () = List.iter (fun block -> this#visit_block block) blocks

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method! visit_block_edge ~block next_block_id =
      prev_blocks <-
        IMap.add next_block_id (ISet.add block.id (IMap.find next_block_id prev_blocks)) prev_blocks

    method add_vreg_def ~(block : virtual_block) vreg =
      if
        VIMMap.contains vreg block.id vreg_use_blocks
        && not (VIMMap.contains vreg block.id vreg_def_blocks)
      then
        vreg_use_before_def_blocks <- VIMMap.add vreg block.id vreg_use_before_def_blocks;
      vreg_def_blocks <- VIMMap.add vreg block.id vreg_def_blocks

    method! visit_instruction ~block instr_with_id =
      let open Instruction in
      let (_, instr) = instr_with_id in
      match instr with
      | CallM _
      | CallL _ ->
        (* Calls define all caller save registers *)
        RegSet.iter
          (fun reg ->
            let color_vreg = RegMap.find reg color_to_vreg in
            this#add_vreg_def ~block color_vreg)
          caller_saved_registers;
        super#visit_instruction ~block instr_with_id
      | _ -> super#visit_instruction ~block instr_with_id

    method! visit_read_vreg ~block vreg_id =
      vreg_use_blocks <- VIMMap.add vreg_id block.id vreg_use_blocks

    method! visit_write_vreg ~block vreg = this#add_vreg_def ~block vreg
  end

let analyze_vregs blocks color_to_vreg =
  (* Calculate use and def blocks for each variable *)
  let init_visitor = new analyze_vregs_init_visitor blocks color_to_vreg in
  init_visitor#run ();

  let prev_blocks = init_visitor#prev_blocks in
  let vreg_use_blocks = init_visitor#vreg_use_blocks in
  let vreg_def_blocks = init_visitor#vreg_def_blocks in
  let vreg_use_before_def_blocks = init_visitor#vreg_use_before_def_blocks in

  (* Initialize liveness sets *)
  let live_in = ref IMap.empty in
  let live_out = ref IMap.empty in
  List.iter
    (fun block ->
      let open Block in
      live_in := IMap.add block.id [] !live_in;
      live_out := IMap.add block.id [] !live_out)
    blocks;

  (* Propagate a single variable backwards through the program, building liveness sets as we go *)
  let set_contains set block_id var_id =
    match IMap.find block_id !set with
    | hd :: _ when hd == var_id -> true
    | _ -> false
  in
  let set_add set block_id var_id =
    set := IMap.add block_id (var_id :: IMap.find block_id !set) !set
  in
  let rec propagate_backwards ~block_id ~vreg_id =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       vreg is defined (unless the vreg is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block_id vreg_id))
      && ( (not (VIMMap.contains vreg_id block_id vreg_def_blocks))
         || VIMMap.contains vreg_id block_id vreg_use_before_def_blocks )
    then (
      set_add live_in block_id vreg_id;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block vreg_id) then set_add live_out prev_block vreg_id;
          propagate_backwards ~block_id:prev_block ~vreg_id)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  VRegMap.iter
    (fun vreg_id use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~vreg_id) use_blocks)
    vreg_use_blocks;

  (!live_in, !live_out)

class analyze_virtual_stack_slots_init_visitor ~(gcx : Gcx.t) =
  object (this)
    inherit X86_visitor.instruction_visitor

    val mutable prev_blocks =
      IMap.fold (fun block_id _ acc -> IMap.add block_id ISet.empty acc) gcx.blocks_by_id IMap.empty

    val mutable vslot_use_blocks = VRegMap.empty

    val mutable vslot_def_blocks = VRegMap.empty

    val mutable vslot_use_before_def_blocks = VRegMap.empty

    method prev_blocks = prev_blocks

    method vslot_use_blocks = vslot_use_blocks

    method vslot_def_blocks = vslot_def_blocks

    method vslot_use_before_def_blocks = vslot_use_before_def_blocks

    method run () = IMap.iter (fun _ block -> this#visit_block block) gcx.blocks_by_id

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method! visit_block_edge ~block next_block_id =
      prev_blocks <-
        IMap.add next_block_id (ISet.add block.id (IMap.find next_block_id prev_blocks)) prev_blocks

    method! visit_read_mem ~block mem =
      let open Instruction in
      match mem with
      | Mem (VirtualStackSlot vreg) -> vslot_use_blocks <- VIMMap.add vreg block.id vslot_use_blocks
      | _ -> ()

    method! visit_write_mem ~block mem =
      let open Instruction in
      match mem with
      | Mem (VirtualStackSlot vreg) ->
        if
          VIMMap.contains vreg block.id vslot_use_blocks
          && not (VIMMap.contains vreg block.id vslot_def_blocks)
        then
          vslot_use_before_def_blocks <- VIMMap.add vreg block.id vslot_use_before_def_blocks;
        vslot_def_blocks <- VIMMap.add vreg block.id vslot_def_blocks
      | _ -> ()
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
  let set_contains set block_id var_id =
    match IMap.find block_id !set with
    | hd :: _ when hd == var_id -> true
    | _ -> false
  in
  let set_add set block_id var_id =
    set := IMap.add block_id (var_id :: IMap.find block_id !set) !set
  in
  let rec propagate_backwards ~block_id ~vreg_id =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       var is defined (unless the var is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block_id vreg_id))
      && ( (not (VIMMap.contains vreg_id block_id vslot_def_blocks))
         || VIMMap.contains vreg_id block_id vslot_use_before_def_blocks )
    then (
      set_add live_in block_id vreg_id;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block vreg_id) then set_add live_out prev_block vreg_id;
          propagate_backwards ~block_id:prev_block ~vreg_id)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  VRegMap.iter
    (fun vreg_id use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~vreg_id) use_blocks)
    vslot_use_blocks;

  (!live_in, !live_out)
