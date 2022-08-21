open Basic_collections
open Mir_type
open X86_64_builders
open X86_64_calling_conventions
open X86_64_instructions
open X86_64_layout
open X86_64_register

let invalid_label_chars = Str.regexp "[-+<>,*():]"

let label_of_mir_label label = Str.global_replace invalid_label_chars "$" label

module Gcx = struct
  type t = {
    (* Virtual blocks and builders *)
    mutable data: data;
    mutable bss: bss;
    mutable current_block_builder: Block.t option;
    mutable current_func_builder: Function.t option;
    (* All blocks, indexed by id *)
    mutable blocks_by_id: Block.t IMap.t;
    mutable prev_blocks: IIMMap.t;
    (* Blocks indexed by their corresponding MIR block. Not all blocks may be in this map. *)
    mutable mir_block_id_to_block_id: Block.id Mir.BlockMap.t;
    mutable mir_func_to_param_types: param_types Mir.FunctionMap.t;
    mutable funcs_by_id: Function.t IMap.t;
    (* Map from physical register to a representative precolored register operand *)
    mutable color_to_op: Operand.t RegMap.t;
    mutable agg_to_layout: AggregateLayout.t IMap.t;
    (* Floating point literals *)
    mutable added_double_negate_mask: bool;
    mutable float_immediates: SSet.t;
    (* Whether the myte_init function has been generated *)
    mutable generated_init_func: bool;
  }

  let mk () =
    (* Initialize representative precolored operands, choosing arbitrary type *)
    let color_to_op =
      RegSet.fold
        (fun reg color_to_op ->
          let type_ =
            if register_class reg == SSEClass then
              Type.Double
            else
              Long
          in
          RegMap.add reg (mk_precolored ~type_ reg) color_to_op)
        all_registers
        RegMap.empty
    in
    {
      data = mk_data_section ();
      bss = mk_data_section ();
      current_block_builder = None;
      current_func_builder = None;
      blocks_by_id = IMap.empty;
      prev_blocks = IIMMap.empty;
      mir_block_id_to_block_id = Mir.BlockMap.empty;
      mir_func_to_param_types = Mir.FunctionMap.empty;
      funcs_by_id = IMap.empty;
      color_to_op;
      agg_to_layout = IMap.empty;
      added_double_negate_mask = false;
      float_immediates = SSet.empty;
      generated_init_func = false;
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

  let add_double_negate_mask ~gcx =
    if not gcx.added_double_negate_mask then (
      let value = SSELiteral [Imm64 Int64.min_int; Imm64 Int64.zero] in
      add_data ~gcx { label = double_negate_mask_label; value; size = 16; is_pointer = false };
      gcx.added_double_negate_mask <- true
    )

  let get_float_literal ~gcx n =
    let float_string = Float.to_string n in
    let literal_label = label_of_mir_label ("_float$" ^ float_string) in
    if not (SSet.mem float_string gcx.float_immediates) then (
      gcx.float_immediates <- SSet.add float_string gcx.float_immediates;
      let value = ImmediateData (Imm64 (Int64.bits_of_float n)) in
      add_data ~gcx { label = literal_label; value; size = 8; is_pointer = false }
    );
    literal_label

  let get_block_id_from_mir_block ~gcx mir_block =
    match Mir.BlockMap.find_opt mir_block gcx.mir_block_id_to_block_id with
    | Some block_id -> block_id
    | None ->
      let block_id = Block.mk_id () in
      gcx.mir_block_id_to_block_id <-
        Mir.BlockMap.add mir_block block_id gcx.mir_block_id_to_block_id;
      block_id

  let start_block ~gcx ~label ~func ~mir_block =
    let id =
      match mir_block with
      | None -> Block.mk_id ()
      | Some mir_block -> get_block_id_from_mir_block ~gcx mir_block
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
    let instruction = mk_instr instr in
    current_block.instructions <- instruction :: current_block.instructions;
    match instr with
    | Jmp next_block_id
    | JmpCC (_, next_block_id) ->
      gcx.prev_blocks <- IIMMap.add next_block_id current_block.id gcx.prev_blocks
    | _ -> ()

  let start_function ~gcx params param_types prologue =
    let id = Function.mk_id () in
    let func =
      {
        Function.id;
        params;
        param_types;
        prologue;
        blocks = [];
        spilled_callee_saved_regs = RegSet.empty;
        spilled_vslots = OperandSet.empty;
        num_stack_frame_slots = 0;
        argument_stack_slots = [];
        num_argument_stack_slots = 0;
      }
    in
    gcx.current_func_builder <- Some func;
    gcx.funcs_by_id <- IMap.add id func gcx.funcs_by_id;
    func

  let finish_function ~gcx =
    let current_func = Option.get gcx.current_func_builder in
    current_func.blocks <- List.rev current_func.blocks;
    gcx.current_func_builder <- None

  let mk_function_argument_stack_slot ~gcx ~i ~type_ =
    (* Return stack slot operand if one already exists for function, otherwise create new stack slot
       operand for function and return it. *)
    let current_func = Option.get gcx.current_func_builder in
    if current_func.num_argument_stack_slots <= i then
      current_func.num_argument_stack_slots <- i + 1;
    let op = mk_function_argument_stack_slot ~i ~type_ in
    current_func.argument_stack_slots <- op :: current_func.argument_stack_slots;
    op

  let get_agg_layout ~gcx agg = IMap.find agg.Aggregate.id gcx.agg_to_layout

  let rec size_of_mir_type ~gcx mir_type =
    match mir_type with
    | Type.Bool
    | Byte ->
      1
    | Short -> 2
    | Int -> 4
    | Long
    | Double
    | Function
    | Pointer _ ->
      8
    | Aggregate agg ->
      let agg_layout = get_agg_layout ~gcx agg in
      agg_layout.size
    | Array (ty, size) -> size_of_mir_type ~gcx ty * size

  let rec alignment_of_mir_type ~gcx mir_type =
    match mir_type with
    | Type.Aggregate agg ->
      let agg_layout = get_agg_layout ~gcx agg in
      agg_layout.alignment
    | Array (ty, _) -> alignment_of_mir_type ~gcx ty
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
        | { instr = Instruction.Jmp next_block_id; _ } :: rev_instrs when next_block_id = block2.id
          ->
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
            (fun { Instruction.instr; _ } ->
              match instr with
              | MovMM (_, op1, op2) ->
                (match (op1.value, op2.value) with
                | (PhysicalRegister reg1, PhysicalRegister reg2) when reg1 = reg2 -> false
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
        | [{ instr = Jmp next_jump_block; _ }] when block.id <> next_jump_block ->
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
        List.iter
          (fun instr ->
            match instr.instr with
            | Jmp next_block_id when IMap.mem next_block_id !jump_aliases ->
              let resolved_alias = resolve_jump_alias next_block_id in
              instr.instr <- Jmp resolved_alias
            | JmpCC (cond, next_block_id) when IMap.mem next_block_id !jump_aliases ->
              let resolved_alias = resolve_jump_alias next_block_id in
              instr.instr <- JmpCC (cond, resolved_alias)
            | _ -> ())
          block.instructions)
      gcx.blocks_by_id
end
