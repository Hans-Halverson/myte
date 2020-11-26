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

(* Break block id and continue block id for a loop *)
type loop_context = Block.id * Block.id

type t = {
  (* Data structures for MIR *)
  mutable main_id: Block.id;
  mutable blocks: BlockBuilder.t IMap.t;
  mutable globals: Global.t LocMap.t;
  mutable funcs: Function.t LocMap.t;
  mutable current_block_builder: BlockBuilder.t option;
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
}

let mk () =
  {
    main_id = 0;
    blocks = IMap.empty;
    globals = LocMap.empty;
    funcs = LocMap.empty;
    current_block_builder = None;
    current_block_sequence_ids = [];
    current_loop_contexts = [];
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

let add_global ~ecx global = ecx.globals <- LocMap.add global.Global.loc global ecx.globals

let add_function ~ecx func = ecx.funcs <- LocMap.add func.Function.loc func ecx.funcs

let is_global_loc ~ecx decl_loc = LocMap.mem decl_loc ecx.globals

let max_instr_id = ref 0

let mk_instr_id () =
  let instr_id = !max_instr_id in
  max_instr_id := instr_id + 1;
  instr_id

let emit ~ecx loc inst =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder -> builder.instructions <- (mk_instr_id (), loc, inst) :: builder.instructions

let emit_phi ~ecx var_id args =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder -> builder.phis <- (var_id, args) :: builder.phis

let mk_block_builder ~ecx =
  let block_id = mk_block_id () in
  let builder = { BlockBuilder.id = block_id; instructions = []; phis = []; next = Halt } in
  ecx.blocks <- IMap.add block_id builder ecx.blocks;
  builder

let set_block_builder ~ecx builder = ecx.current_block_builder <- Some builder

let start_new_block ~ecx =
  let builder = mk_block_builder ~ecx in
  set_block_builder ~ecx builder;
  builder.id

let finish_block ~ecx next =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder ->
    builder.next <- next;
    ecx.current_block_sequence_ids <- builder.id :: ecx.current_block_sequence_ids;
    ecx.current_block_builder <- None

let finish_block_branch ~ecx test continue jump =
  finish_block ~ecx (Branch { test; continue; jump })

let finish_block_continue ~ecx continue = finish_block ~ecx (Continue continue)

let finish_block_halt ~ecx = finish_block ~ecx Halt

let start_block_sequence ~ecx = ecx.current_block_sequence_ids <- []

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids

let push_loop_context ~ecx break_id continue_id =
  ecx.current_loop_contexts <- (break_id, continue_id) :: ecx.current_loop_contexts

let pop_loop_context ~ecx = ecx.current_loop_contexts <- List.tl ecx.current_loop_contexts

let get_loop_context ~ecx = List.hd ecx.current_loop_contexts
