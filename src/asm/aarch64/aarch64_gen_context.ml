open Aarch64_asm
open Aarch64_calling_convention
open Aarch64_register
open Asm
open Asm_builders
open Asm_calling_convention
open Asm_codegen
open Asm_instruction_definition
open Asm_register
open Mir_type

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
  }

  let mk () =
    (* Initialize representative precolored operands, choosing arbitrary type *)
    let color_to_op =
      RegSet.fold
        (fun reg color_to_op -> RegMap.add reg (mk_precolored ~type_:Type.Long reg) color_to_op)
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
    }

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

  let mir_function_calling_convention (_func : Mir.Function.t) = aapcs64

  let start_function ~gcx ~ir (func : Mir.Function.t) param_types =
    let label = get_asm_function_label ~ir func in
    let calling_convention = mir_function_calling_convention func in
    let func = mk_function ~label ~param_types ~return_type:func.return_type ~calling_convention in
    gcx.current_func <- Some func;
    gcx.funcs <- FunctionSet.add func gcx.funcs;
    func

  let finish_function ~gcx =
    let current_func = Option.get gcx.current_func in
    current_func.blocks <- List.rev current_func.blocks;
    gcx.current_func <- None

  let emit ~gcx (instr : AArch64.instr) operands =
    let instr = (instr :> instr) in
    let current_block = Option.get gcx.current_block in
    mk_instr_ ~block:current_block instr operands;
    match (instr, operands) with
    | (`B, [| { value = Block next_block; _ } |]) ->
      gcx.prev_blocks <- BlockMMap.add next_block current_block gcx.prev_blocks
    | _ -> ()
end
