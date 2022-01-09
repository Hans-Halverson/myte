open Basic_collections
open Mir_type
open X86_64_instructions
open X86_64_layout

module Gcx = struct
  type t = {
    (* Virtual blocks and builders *)
    mutable data: data;
    mutable bss: bss;
    mutable current_block_builder: virtual_block option;
    mutable current_func_builder: VReg.t Function.t option;
    (* All blocks, indexed by id *)
    mutable blocks_by_id: virtual_block IMap.t;
    mutable prev_blocks: IIMMap.t;
    (* Blocks indexed by their corresponding MIR block. Not all blocks may be in this map. *)
    mutable mir_block_id_to_block_id: Block.id IMap.t;
    (* Number of uses of each MIR variable *)
    mutable var_num_uses: int IMap.t;
    (* Map from instruction id to the block that contains it *)
    mutable instruction_to_block: Block.id IMap.t;
    mutable funcs_by_id: VReg.t Function.t IMap.t;
    (* Map from physical register to a precolored virtual register *)
    mutable color_to_vreg: VReg.t RegMap.t;
    mutable agg_to_layout: AggregateLayout.t IMap.t;
  }

  let mk () =
    (* Initialize representative precolored vregs *)
    let (color_to_vreg, _precolored_vregs, _interference_degree) =
      RegSet.fold
        (fun reg (color_to_vreg, precolored_vregs, interference_degree) ->
          let vreg = VirtualRegister.mk ~resolution:(Physical reg) in
          ( RegMap.add reg vreg color_to_vreg,
            VRegSet.add vreg precolored_vregs,
            VRegMap.add vreg Int.max_int interference_degree ))
        all_registers
        (RegMap.empty, VRegSet.empty, VRegMap.empty)
    in
    {
      data = mk_data_section ();
      bss = mk_data_section ();
      current_block_builder = None;
      current_func_builder = None;
      blocks_by_id = IMap.empty;
      prev_blocks = IIMMap.empty;
      mir_block_id_to_block_id = IMap.empty;
      var_num_uses = IMap.empty;
      instruction_to_block = IMap.empty;
      funcs_by_id = IMap.empty;
      color_to_vreg;
      agg_to_layout = IMap.empty;
    }

  let finish_builders ~gcx =
    gcx.data <- Array.map List.rev gcx.data;
    gcx.bss <- Array.map List.rev gcx.bss

  let add_data ~gcx init_data =
    let align_index = align_to_data_section_align_index (align_of_data_value init_data.value) in
    gcx.data.(align_index) <- init_data :: gcx.data.(align_index)

  let add_bss ~gcx uninit_data align =
    let align_index = align_to_data_section_align_index align in
    gcx.bss.(align_index) <- uninit_data :: gcx.bss.(align_index)

  let get_block_id_from_mir_block_id ~gcx mir_block_id =
    match IMap.find_opt mir_block_id gcx.mir_block_id_to_block_id with
    | Some block_id -> block_id
    | None ->
      let block_id = Block.mk_id () in
      gcx.mir_block_id_to_block_id <- IMap.add mir_block_id block_id gcx.mir_block_id_to_block_id;
      block_id

  let start_block ~gcx ~label ~func ~mir_block_id =
    let id =
      match mir_block_id with
      | None -> Block.mk_id ()
      | Some mir_block_id -> get_block_id_from_mir_block_id ~gcx mir_block_id
    in
    gcx.current_block_builder <- Some { id; label; func; instructions = [] }

  let finish_block ~gcx =
    let block = Option.get gcx.current_block_builder in
    block.instructions <- List.rev block.instructions;
    gcx.blocks_by_id <- IMap.add block.id block gcx.blocks_by_id;
    let func = IMap.find block.func gcx.funcs_by_id in
    func.blocks <- block :: func.blocks;
    gcx.current_block_builder <- None

  let emit ~gcx instr =
    let current_block = Option.get gcx.current_block_builder in
    let instr_id = Instruction.mk_id () in
    current_block.instructions <- (instr_id, instr) :: current_block.instructions;
    gcx.instruction_to_block <- IMap.add instr_id current_block.id gcx.instruction_to_block;
    match instr with
    | Jmp next_block_id
    | JmpCC (_, next_block_id) ->
      gcx.prev_blocks <- IIMMap.add next_block_id current_block.id gcx.prev_blocks
    | _ -> ()

  let mk_instr_id_for_block ~gcx block =
    let instr_id = Instruction.mk_id () in
    gcx.instruction_to_block <- IMap.add instr_id block.Block.id gcx.instruction_to_block;
    instr_id

  let mk_precolored ~gcx color = RegMap.find color gcx.color_to_vreg

  let start_function ~gcx params prologue =
    let id = Function.mk_id () in
    let func =
      {
        Function.id;
        params;
        prologue;
        blocks = [];
        spilled_callee_saved_regs = RegSet.empty;
        spilled_vregs = VRegSet.empty;
        num_stack_frame_slots = 0;
        argument_stack_slots = [];
      }
    in
    gcx.current_func_builder <- Some func;
    gcx.funcs_by_id <- IMap.add id func gcx.funcs_by_id;
    func

  let finish_function ~gcx =
    let current_func = Option.get gcx.current_func_builder in
    current_func.blocks <- List.rev current_func.blocks;
    gcx.current_func_builder <- None

  let get_current_argument_stack_slot ~gcx i =
    (* Return stack slot vreg if one already exists for function, otherwise create new stack slot
       vreg for function and return it. *)
    let current_func = Option.get gcx.current_func_builder in
    match List.nth_opt current_func.argument_stack_slots i with
    | Some stack_slot_vreg -> stack_slot_vreg
    | None ->
      let stack_slot_vreg = VReg.mk ~resolution:Unresolved in
      stack_slot_vreg.resolution <- StackSlot (FunctionArgumentStackSlot stack_slot_vreg);
      current_func.argument_stack_slots <- current_func.argument_stack_slots @ [stack_slot_vreg];
      stack_slot_vreg

  let get_instruction ~gcx instr_id =
    let block_id = IMap.find instr_id gcx.instruction_to_block in
    let block = IMap.find block_id gcx.blocks_by_id in
    let (_, instr) = List.find (fun (id, _) -> id = instr_id) block.instructions in
    instr

  let get_agg_layout ~gcx agg = IMap.find agg.Aggregate.id gcx.agg_to_layout

  let rec size_of_mir_type ~gcx mir_type =
    match mir_type with
    | `BoolT
    | `ByteT ->
      1
    | `IntT -> 4
    | `LongT
    | `FunctionT
    | `PointerT _ ->
      8
    | `AggregateT agg ->
      let agg_layout = get_agg_layout ~gcx agg in
      agg_layout.size
    | `ArrayT (ty, size) -> size_of_mir_type ~gcx ty * size

  let rec alignment_of_mir_type ~gcx mir_type =
    match mir_type with
    | `AggregateT agg ->
      let agg_layout = get_agg_layout ~gcx agg in
      agg_layout.alignment
    | `ArrayT (ty, _) -> alignment_of_mir_type ~gcx ty
    | _ -> size_of_mir_type ~gcx mir_type

  let build_agg_layout ~gcx agg =
    let open Aggregate in
    let current_offset = ref 0 in
    let largest_alignment = ref 0 in

    (* Calculate offsets for each aggregate element *)
    let elements =
      List.map
        (fun (_, ty) ->
          let size = size_of_mir_type ~gcx ty in
          let alignment = alignment_of_mir_type ~gcx ty in
          largest_alignment := max alignment !largest_alignment;

          (* Add padding to align element *)
          let align_overflow = !current_offset mod alignment in
          if align_overflow <> 0 then current_offset := !current_offset + (size - align_overflow);

          let offset = !current_offset in
          current_offset := offset + size;
          { AggregateElement.offset; size })
        agg.elements
    in

    (* Aggregate has alignment of its largest element. Aggregate size must be multiple of alignment,
       so insert padding to end of aggregate if necessary. *)
    let alignment = !largest_alignment in
    let align_overflow =
      if alignment = 0 then
        0
      else
        !current_offset mod alignment
    in
    if align_overflow <> 0 then current_offset := !current_offset + (alignment - align_overflow);

    let agg_layout =
      { AggregateLayout.agg; size = !current_offset; alignment; elements = Array.of_list elements }
    in
    gcx.agg_to_layout <- IMap.add agg.id agg_layout gcx.agg_to_layout

  let remove_redundant_instructions ~gcx =
    let open Block in
    (* Remove jump instructions where the label immediately succeeds the jump instruction *)
    let rec merge blocks =
      match blocks with
      | block1 :: block2 :: tl ->
        (match List.rev block1.instructions with
        | (_, Instruction.Jmp next_block_id) :: rev_instrs when next_block_id = block2.id ->
          block1.instructions <- List.rev rev_instrs
        | _ -> ());
        merge (block2 :: tl)
      | _ -> ()
    in
    IMap.iter (fun _ func -> merge func.Function.blocks) gcx.funcs_by_id;
    (* Remove reflexive move instructions *)
    IMap.iter
      (fun _ block ->
        let open Instruction in
        block.instructions <-
          List.filter
            (fun (_, instr) ->
              match instr with
              | MovMM (_, Reg vreg1, Reg vreg2) ->
                (match (VReg.get_vreg_resolution vreg1, VReg.get_vreg_resolution vreg2) with
                | (Physical reg1, Physical reg2) when reg1 = reg2 -> false
                | _ -> true)
              | _ -> true)
            block.instructions)
      gcx.blocks_by_id

  (* Find all empty blocks that only contain an unconditional jump. Remove them and rewrite other
     jumps in graph to skip over them (may skip an entire chain). *)
  let compress_jump_aliases ~gcx =
    let open Block in
    (* Find all jump aliases in program *)
    let jump_aliases = ref IMap.empty in
    IMap.iter
      (fun _ block ->
        match block.instructions with
        | [(_, Jmp next_jump_block)] when block.id <> next_jump_block ->
          jump_aliases := IMap.add block.id next_jump_block !jump_aliases
        | _ -> ())
      gcx.blocks_by_id;
    (* Filter out jump alias blocks *)
    IMap.iter
      (fun _ func ->
        let open Function in
        func.blocks <-
          List.filter
            (fun block ->
              let func = IMap.find block.func gcx.funcs_by_id in
              if IMap.mem block.id !jump_aliases && func.prologue != block.id then (
                gcx.blocks_by_id <- IMap.remove block.id gcx.blocks_by_id;
                gcx.prev_blocks <- IIMMap.remove_key block.id gcx.prev_blocks;
                false
              ) else
                true)
            func.blocks)
      gcx.funcs_by_id;
    (* Rewrite jumps to skip over jump alias blocks *)
    let rec resolve_jump_alias block_id =
      match IMap.find_opt block_id !jump_aliases with
      | None -> block_id
      | Some alias -> resolve_jump_alias alias
    in
    IMap.iter
      (fun _ block ->
        let open Instruction in
        block.instructions <-
          List.map
            (fun ((instr_id, instr) as instr_with_id) ->
              match instr with
              | Jmp next_block_id when IMap.mem next_block_id !jump_aliases ->
                let resolved_alias = resolve_jump_alias next_block_id in
                (instr_id, Jmp resolved_alias)
              | JmpCC (cond, next_block_id) when IMap.mem next_block_id !jump_aliases ->
                let resolved_alias = resolve_jump_alias next_block_id in
                (instr_id, JmpCC (cond, resolved_alias))
              | _ -> instr_with_id)
            block.instructions)
      gcx.blocks_by_id
end
