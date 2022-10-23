open Aarch64_asm
open Aarch64_register
open Asm
open Asm_builders
open Asm_calling_convention
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
end
