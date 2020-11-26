open Basic_collections
open Mir

type instr_id = int

module BlockBuilder = struct
  type t = {
    id: Block.id;
    (* Instructions in the block currently being built, in reverse *)
    mutable instructions: (instr_id * Loc.t * cf_instruction) list;
    mutable phis: (cf_var * cf_var list) list;
    mutable next: cf_var Block.next;
  }
end

type t = {
  (* Data structures for MIR *)
  mutable main_id: Block.id;
  mutable blocks: BlockBuilder.t IMap.t;
  mutable globals: Global.t LocMap.t;
  mutable funcs: Function.t LocMap.t;
  mutable current_block_builder: BlockBuilder.t;
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
}

let mk () =
  {
    main_id = 0;
    blocks = IMap.empty;
    globals = LocMap.empty;
    funcs = LocMap.empty;
    current_block_builder = { id = 0; phis = []; instructions = []; next = Halt };
    current_block_sequence_ids = [];
  }

let builders_to_blocks builders =
  IMap.map
    (fun builder ->
      {
        Block.id = builder.BlockBuilder.id;
        instructions =
          List.rev (List.map (fun (_, loc, instr) -> (loc, instr)) builder.instructions);
        phis = builder.phis;
        next = builder.next;
      })
    builders

let add_block ~ecx block_builder =
  ecx.blocks <- IMap.add block_builder.BlockBuilder.id block_builder ecx.blocks

let add_global ~ecx global = ecx.globals <- LocMap.add global.Global.loc global ecx.globals

let add_function ~ecx func = ecx.funcs <- LocMap.add func.Function.loc func ecx.funcs

let is_global_loc ~ecx decl_loc = LocMap.mem decl_loc ecx.globals

let max_instr_id = ref 0

let mk_instr_id () =
  let instr_id = !max_instr_id in
  max_instr_id := instr_id + 1;
  instr_id

let emit ~ecx loc inst =
  ecx.current_block_builder.instructions <-
    (mk_instr_id (), loc, inst) :: ecx.current_block_builder.instructions

let emit_phi ~ecx var_id args =
  ecx.current_block_builder.phis <- (var_id, args) :: ecx.current_block_builder.phis

let mk_block_builder () =
  { BlockBuilder.id = mk_block_id (); instructions = []; phis = []; next = Halt }

let set_block_builder ~ecx builder = ecx.current_block_builder <- builder

let start_block ~ecx =
  let block_id = mk_block_id () in
  ecx.current_block_builder <- { id = block_id; instructions = []; phis = []; next = Halt };
  block_id

let finish_block ~ecx next =
  let builder = ecx.current_block_builder in
  builder.next <- next;
  add_block ~ecx builder;
  ecx.current_block_sequence_ids <- builder.id :: ecx.current_block_sequence_ids

let finish_block_branch ~ecx test continue jump =
  finish_block ~ecx (Branch { test; continue; jump })

let finish_block_continue ~ecx continue = finish_block ~ecx (Continue continue)

let finish_block_halt ~ecx = finish_block ~ecx Halt

let start_block_sequence ~ecx = ecx.current_block_sequence_ids <- []

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids
