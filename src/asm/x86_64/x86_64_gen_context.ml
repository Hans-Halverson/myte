open Asm
open Asm_builders
open Asm_calling_convention
open Asm_codegen
open Asm_instruction_definition
open Asm_register
open Basic_collections
open Mir_type
open X86_64_asm
open X86_64_calling_convention
open X86_64_layout
open X86_64_register

module Gcx = struct
  type t = {
    mutable funcs: FunctionSet.t;
    mutable data: data;
    mutable bss: bss;
    mutable current_block: Block.t option;
    mutable current_func: Function.t option;
    mutable prev_blocks: BlockMMap.t;
    (* Blocks indexed by their corresponding MIR block. Not all blocks may be in this map. *)
    mutable mir_block_to_block: Block.t Mir.BlockMap.t;
    mutable mir_func_to_param_types: param_types Mir.FunctionMap.t;
    (* Map from physical register to a representative precolored register operand *)
    mutable color_to_op: Operand.t RegMap.t;
    mutable agg_to_layout: AggregateLayout.t IMap.t;
    (* Floating point literals *)
    mutable added_double_negate_mask: bool;
    mutable float_immediates: SSet.t;
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
      current_block = None;
      current_func = None;
      prev_blocks = BlockMMap.empty;
      mir_block_to_block = Mir.BlockMap.empty;
      mir_func_to_param_types = Mir.FunctionMap.empty;
      funcs = FunctionSet.empty;
      color_to_op;
      agg_to_layout = IMap.empty;
      added_double_negate_mask = false;
      float_immediates = SSet.empty;
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

  let get_block_from_mir_block ~gcx mir_block =
    match Mir.BlockMap.find_opt mir_block gcx.mir_block_to_block with
    | Some block -> block
    | None ->
      let block = mk_block ~func:null_function in
      gcx.mir_block_to_block <- Mir.BlockMap.add mir_block block gcx.mir_block_to_block;
      block

  let start_block ~gcx ~label ~func ~mir_block =
    let block =
      match mir_block with
      | None -> mk_block ~func
      | Some mir_block -> get_block_from_mir_block ~gcx mir_block
    in
    block.func <- func;
    block.label <- label;
    gcx.current_block <- Some block

  let finish_block ~gcx =
    let block = Option.get gcx.current_block in
    block.func.blocks <- block :: block.func.blocks;
    gcx.current_block <- None

  let emit ~gcx (instr : X86_64.instr) operands =
    let instr = (instr :> instr) in
    let current_block = Option.get gcx.current_block in
    mk_instr_ ~block:current_block instr operands;
    match (instr, operands) with
    | (`Jmp, [| { value = Block next_block; _ } |])
    | (`JmpCC _, [| { value = Block next_block; _ } |]) ->
      gcx.prev_blocks <- BlockMMap.add next_block current_block gcx.prev_blocks
    | _ -> ()

  let mir_function_calling_convention (_func : Mir.Function.t) = system_v_calling_convention

  let start_function ~gcx (func : Mir.Function.t) param_types =
    let calling_convention = mir_function_calling_convention func in
    let func = mk_function ~param_types ~return_type:func.return_type ~calling_convention in
    gcx.current_func <- Some func;
    gcx.funcs <- FunctionSet.add func gcx.funcs;
    func

  let finish_function ~gcx =
    let current_func = Option.get gcx.current_func in
    current_func.blocks <- List.rev current_func.blocks;
    gcx.current_func <- None

  let mk_function_argument_stack_slot ~gcx ~i ~type_ =
    (* Return stack slot operand if one already exists for function, otherwise create new stack slot
       operand for function and return it. *)
    let current_func = Option.get gcx.current_func in
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

  (* Simplify Jmp instructions, removing unnecessary Jmp instructions when possible *)
  let simplify_jumps ~gcx =
    let rec merge blocks =
      match blocks with
      | block1 :: block2 :: tl ->
        (match get_last_instr_opt block1 with
        | Some ({ instr = `Jmp; operands = [| { value = Block next_block; _ } |]; _ } as jmp_instr)
          when next_block.id = block2.id ->
          remove_instruction jmp_instr
        (* Eliminate unnecessary Jmp instructions by reordering sequences of JmpCCs:

             JmpCC block1
             Jmp block2           JmpCC block2 (with inverse CC)
           block1:              block1:
             ...                  ...
           block2:              block2:
             ...                  ... *)
        | Some
            ({
               instr = `Jmp;
               operands = [| { value = Block jmp_block; _ } |];
               prev =
                 { instr = `JmpCC cc; operands = [| { value = Block jmp_cc_block; _ } |]; _ } as
                 jmp_cc_instr;
               _;
             } as jmp_instr)
          when jmp_cc_block.id == block2.id ->
          remove_instruction jmp_instr;
          jmp_cc_instr.instr <- `JmpCC (invert_condition_code cc);
          jmp_cc_instr.operands.(0) <- mk_block_op ~block:jmp_block
        | _ -> ());
        merge (block2 :: tl)
      | _ -> ()
    in
    FunctionSet.iter (fun func -> merge func.blocks) gcx.funcs

  let remove_redundant_moves ~gcx =
    (* Remove reflexive move instructions *)
    FunctionSet.iter
      (fun func ->
        func_iter_blocks func (fun block ->
            filter_instructions block (fun { Instruction.instr; operands; _ } ->
                match (instr, operands) with
                | (`MovMM _, [| op1; op2 |]) ->
                  (match (op1.value, op2.value) with
                  | (PhysicalRegister reg1, PhysicalRegister reg2) when reg1 = reg2 -> false
                  | _ -> true)
                | _ -> true)))
      gcx.funcs

  (* Find all empty blocks that only contain an unconditional jump. Remove them and rewrite other
     jumps in graph to skip over them (may skip an entire chain). *)
  let compress_jump_aliases ~gcx =
    let open Block in
    (* Find all jump aliases in program *)
    let jump_aliases = ref BlockMap.empty in
    funcs_iter_blocks gcx.funcs (fun block ->
        match get_first_instr_opt block with
        | Some { instr = `Jmp; operands = [| { value = Block next_jump_block; _ } |]; _ }
          when has_single_instruction block && block.id != next_jump_block.id ->
          jump_aliases := BlockMap.add block next_jump_block !jump_aliases
        | _ -> ());
    (* Filter out jump alias blocks *)
    FunctionSet.iter
      (fun func ->
        let open Function in
        func.blocks <-
          List.filter
            (fun block ->
              if BlockMap.mem block !jump_aliases && block.func.prologue != block then (
                gcx.prev_blocks <- BlockMMap.remove_key block gcx.prev_blocks;
                false
              ) else
                true)
            func.blocks)
      gcx.funcs;
    (* Rewrite jumps to skip over jump alias blocks *)
    let rec resolve_jump_alias block =
      match BlockMap.find_opt block !jump_aliases with
      | None -> block
      | Some alias -> resolve_jump_alias alias
    in
    funcs_iter_blocks gcx.funcs (fun block ->
        let open Instruction in
        iter_instructions block (fun instr ->
            match instr with
            | { instr = `Jmp; operands = [| { value = Block next_block; _ } as block_op |]; _ }
              when BlockMap.mem next_block !jump_aliases ->
              let resolved_alias = resolve_jump_alias next_block in
              if resolved_alias != next_block then block_op.value <- Block resolved_alias
            | { instr = `JmpCC _; operands = [| { value = Block next_block; _ } as block_op |]; _ }
              when BlockMap.mem next_block !jump_aliases ->
              let resolved_alias = resolve_jump_alias next_block in
              if resolved_alias != next_block then block_op.value <- Block resolved_alias
            | _ -> ()))
end
