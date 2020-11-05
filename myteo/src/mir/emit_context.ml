open Basic_collections
open Mir

type t = {
  (* Data structures for MIR *)
  main_id: Block.id;
  mutable blocks: Block.t IMap.t;
  mutable globals: Global.t LocMap.t;
  mutable funcs: Function.t LocMap.t;
  (* Instructions in the block currently being built, in reverse *)
  mutable current_instructions: (Loc.t * Instruction.t) list;
}

let mk () =
  {
    main_id = mk_block_id ();
    blocks = IMap.empty;
    globals = LocMap.empty;
    funcs = LocMap.empty;
    current_instructions = [];
  }

let add_global ~ecx loc id init = ecx.globals <- LocMap.add loc { Global.loc; id; init } ecx.globals

let add_block ~ecx block = ecx.blocks <- IMap.add block.Block.id block ecx.blocks

let emit ~ecx loc inst = ecx.current_instructions <- (loc, inst) :: ecx.current_instructions

let finish_block_halt ~ecx ?(debug = false) label =
  let label =
    if debug then
      Block.DebugLabel label
    else
      Block.Label label
  in
  let block = {
    Block.id = mk_block_id ();
    label;
    instructions = List.rev ecx.current_instructions;
    next = Halt;
  } in
  ecx.current_instructions <- [];
  block
