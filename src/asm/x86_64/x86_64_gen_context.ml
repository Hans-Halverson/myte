open Asm
open Asm_builders
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
    (* Functions indexed by their corresponding MIR function. *)
    mutable mir_func_to_func: Function.t Mir.FunctionMap.t;
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
      mir_func_to_func = Mir.FunctionMap.empty;
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

  let add_function ~gcx ~ir (mir_func : Mir.Function.t) param_types =
    let label = get_asm_function_label ~ir mir_func in
    let calling_convention = mir_function_calling_convention mir_func in
    let func =
      mk_function ~label ~param_types ~return_type:mir_func.return_type ~calling_convention
    in
    gcx.funcs <- FunctionSet.add func gcx.funcs;
    gcx.mir_func_to_func <- Mir.FunctionMap.add mir_func func gcx.mir_func_to_func

  let get_func_from_mir_func ~gcx mir_func = Mir.FunctionMap.find mir_func gcx.mir_func_to_func

  let start_function ~gcx func =
    gcx.current_func <- Some func;
    gcx.mir_block_to_block <- Mir.BlockMap.empty

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
end
