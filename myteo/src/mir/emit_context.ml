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
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
}

let mk () =
  {
    main_id = mk_block_id ();
    blocks = IMap.empty;
    globals = LocMap.empty;
    funcs = LocMap.empty;
    current_instructions = [];
    current_block_sequence_ids = [];
  }

let add_block ~ecx block = ecx.blocks <- IMap.add block.Block.id block ecx.blocks

let add_global ~ecx global = ecx.globals <- LocMap.add global.Global.loc global ecx.globals

let add_function ~ecx func = ecx.funcs <- LocMap.add func.Function.loc func ecx.funcs

let emit ~ecx loc inst = ecx.current_instructions <- (loc, inst) :: ecx.current_instructions

let finish_block_halt ~ecx =
  let block_id = mk_block_id () in
  let block =
    { Block.id = block_id; instructions = List.rev ecx.current_instructions; next = Halt }
  in
  add_block ~ecx block;
  ecx.current_instructions <- [];
  ecx.current_block_sequence_ids <- block_id :: ecx.current_block_sequence_ids

let start_block_sequence ~ecx =
  ecx.current_instructions <- [];
  ecx.current_block_sequence_ids <- []

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids
