open Basic_collections
open X86_instructions

let add_to_multimap key value mmap =
  let new_values =
    match IMap.find_opt key mmap with
    | None -> ISet.singleton value
    | Some values -> ISet.add value values
  in
  IMap.add key new_values mmap

let remove_from_multimap key value mmap =
  let new_values =
    match IMap.find_opt key mmap with
    | None -> ISet.empty
    | Some values -> ISet.remove value values
  in
  IMap.add key new_values mmap

let get_from_multimap key mmap =
  match IMap.find_opt key mmap with
  | None -> ISet.empty
  | Some value -> value

module Gcx = struct
  type t = {
    (* Virtual blocks and builders *)
    mutable data: data list;
    mutable bss: bss_data list;
    mutable rodata: data list;
    mutable current_block_builder: virtual_block option;
    mutable current_func_builder: VReg.t Function.t option;
    (* All blocks, indexed by id *)
    mutable blocks_by_id: virtual_block IMap.t;
    mutable prev_blocks: ISet.t IMap.t;
    (* Blocks indexed by their corresponding MIR block. Not all blocks may be in this map. *)
    mutable mir_block_id_to_block_id: Block.id IMap.t;
    (* Map from instruction id to the block that contains it *)
    mutable instruction_to_block: Block.id IMap.t;
    mutable funcs_by_id: VReg.t Function.t IMap.t;
    mutable max_string_literal_id: int;
    (* Data structures used for register allocation *)
    (* Map of virtual registers live at the beginning of each block *)
    mutable live_out: VReg.t list IMap.t;
    (* Every virtual register is in exactly one of these sets *)
    mutable precolored_vregs: VRegSet.t;
    mutable initial_vregs: VRegSet.t;
    (* Low degree, non move related vregs *)
    mutable simplify_worklist: VRegSet.t;
    mutable freeze_worklist: VRegSet.t;
    mutable spill_worklist: VRegSet.t;
    mutable spilled_vregs: VRegSet.t;
    mutable coalesced_vregs: VRegSet.t;
    mutable colored_vregs: VRegSet.t;
    (* Stack of vregs that are ready for an attempt to have colors assigned to them *)
    mutable select_stack: VReg.t list;
    (* Every move is in exactly one of these sets *)
    mutable coalesced_moves: ISet.t;
    mutable constrained_moves: ISet.t;
    mutable frozen_moves: ISet.t;
    (* Candidates for coalescing *)
    mutable worklist_moves: ISet.t;
    mutable active_moves: ISet.t;
    (* Adjacency list representation of interference graph. Maps from virtual register to a set of
       all virtual registers that interfere with it. *)
    mutable interference_graph: VRegSet.t VRegMap.t;
    (* Degree of each virtual register in interference graph *)
    mutable interference_degree: int VRegMap.t;
    (* Map from virtual register to instruction ids of all moves it is a part of *)
    mutable move_list: ISet.t VRegMap.t;
    (* Map from physical register to a precolored virtual register *)
    mutable color_to_vreg: VReg.t RegMap.t;
  }

  let mk () =
    (* Initialize representative precolored vregs *)
    let (color_to_vreg, precolored_vregs, interference_degree) =
      RegSet.fold
        (fun reg (color_to_vreg, precolored_vregs, interference_degree) ->
          let vreg = VirtualRegister.mk ~func:None ~resolution:(Physical reg) in
          ( RegMap.add reg vreg color_to_vreg,
            VRegSet.add vreg precolored_vregs,
            VRegMap.add vreg Int.max_int interference_degree ))
        all_registers
        (RegMap.empty, VRegSet.empty, VRegMap.empty)
    in
    {
      data = [];
      bss = [];
      rodata = [];
      current_block_builder = None;
      current_func_builder = None;
      blocks_by_id = IMap.empty;
      prev_blocks = IMap.empty;
      mir_block_id_to_block_id = IMap.empty;
      instruction_to_block = IMap.empty;
      funcs_by_id = IMap.empty;
      max_string_literal_id = 0;
      live_out = IMap.empty;
      precolored_vregs;
      initial_vregs = VRegSet.empty;
      simplify_worklist = VRegSet.empty;
      freeze_worklist = VRegSet.empty;
      spill_worklist = VRegSet.empty;
      spilled_vregs = VRegSet.empty;
      coalesced_vregs = VRegSet.empty;
      colored_vregs = VRegSet.empty;
      select_stack = [];
      coalesced_moves = ISet.empty;
      constrained_moves = ISet.empty;
      frozen_moves = ISet.empty;
      worklist_moves = ISet.empty;
      active_moves = ISet.empty;
      interference_graph = VRegMap.empty;
      interference_degree;
      move_list = VRegMap.empty;
      color_to_vreg;
    }

  let finish_builders ~gcx =
    gcx.data <- List.rev gcx.data;
    gcx.bss <- List.rev gcx.bss;
    gcx.rodata <- List.rev gcx.rodata

  let add_data ~gcx d = gcx.data <- d :: gcx.data

  let add_string_literal ~gcx ?label str =
    let label =
      match label with
      | None ->
        let id = gcx.max_string_literal_id in
        gcx.max_string_literal_id <- id + 1;
        ".S" ^ string_of_int id
      | Some label -> label
    in
    let data = { label; value = AsciiData str } in
    gcx.rodata <- data :: gcx.rodata;
    label

  let add_bss ~gcx bd = gcx.bss <- bd :: gcx.bss

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
      gcx.prev_blocks <- add_to_multimap next_block_id current_block.id gcx.prev_blocks
    | _ -> ()

  let mk_instr_id_for_block ~gcx block =
    let instr_id = Instruction.mk_id () in
    gcx.instruction_to_block <- IMap.add instr_id block.Block.id gcx.instruction_to_block;
    instr_id

  let mk_precolored ~gcx color = RegMap.find color gcx.color_to_vreg

  let rec get_vreg_alias ~gcx vreg =
    let open VReg in
    match vreg.resolution with
    | Alias alias when VRegSet.mem vreg gcx.coalesced_vregs -> get_vreg_alias ~gcx alias
    | _ -> vreg

  let get_vreg_resolution ~gcx vreg = (get_vreg_alias ~gcx vreg).resolution

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
      }
    in
    gcx.current_func_builder <- Some func;
    gcx.funcs_by_id <- IMap.add id func gcx.funcs_by_id;
    func

  let finish_function ~gcx =
    let current_func = Option.get gcx.current_func_builder in
    current_func.blocks <- List.rev current_func.blocks;
    gcx.current_func_builder <- None

  let get_instruction ~gcx instr_id =
    let block_id = IMap.find instr_id gcx.instruction_to_block in
    let block = IMap.find block_id gcx.blocks_by_id in
    let (_, instr) = List.find (fun (id, _) -> id = instr_id) block.instructions in
    instr

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
              | MovMM (Reg vreg1, Reg vreg2) ->
                (match (get_vreg_resolution ~gcx vreg1, get_vreg_resolution ~gcx vreg2) with
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
        | [(_, Jmp next_jump_block)] ->
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
                gcx.prev_blocks <- IMap.remove block.id gcx.prev_blocks;
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
