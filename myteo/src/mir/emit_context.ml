open Basic_collections
open Mir

module BlockBuilder = struct
  type t = {
    id: Block.id;
    source: Block.source;
    (* Instructions in the block currently being built, in reverse *)
    mutable instructions: cf_instruction list;
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
  mutable globals: Global.t SMap.t;
  mutable funcs: Function.t SMap.t;
  mutable modules: Module.t SMap.t;
  mutable current_block_builder: BlockBuilder.t option;
  mutable current_module_builder: Module.t option;
  mutable current_block_source: Block.source;
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
  (* Stack of loop contexts for all loops we are currently inside *)
  mutable current_loop_contexts: loop_context list;
}

let mk () =
  {
    main_id = 0;
    blocks = IMap.empty;
    globals = SMap.empty;
    funcs = SMap.empty;
    modules = SMap.empty;
    current_block_builder = None;
    current_module_builder = None;
    current_block_source = GlobalInit "";
    current_block_sequence_ids = [];
    current_loop_contexts = [];
  }

let builders_to_blocks builders =
  IMap.map
    (fun builder ->
      {
        Block.id = builder.BlockBuilder.id;
        instructions = List.rev builder.instructions;
        phis = builder.phis;
        next = builder.next;
        source = builder.source;
      })
    builders

let add_global ~ecx global =
  let name = global.Global.name in
  let builder = Option.get ecx.current_module_builder in
  ecx.globals <- SMap.add name global ecx.globals;
  builder.globals <- SSet.add name builder.globals

let add_function ~ecx func =
  let name = func.Function.name in
  let builder = Option.get ecx.current_module_builder in
  ecx.funcs <- SMap.add name func ecx.funcs;
  builder.funcs <- SSet.add name builder.funcs

let emit ~ecx inst =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder -> builder.instructions <- (mk_instr_id (), inst) :: builder.instructions

let emit_phi ~ecx var_id args =
  match ecx.current_block_builder with
  | None -> ()
  | Some builder -> builder.phis <- (var_id, args) :: builder.phis

let mk_block_builder ~ecx =
  let block_id = mk_block_id () in
  let builder =
    {
      BlockBuilder.id = block_id;
      source = ecx.current_block_source;
      instructions = [];
      phis = [];
      next = Halt;
    }
  in
  ecx.blocks <- IMap.add block_id builder ecx.blocks;
  builder

let set_block_builder ~ecx builder = ecx.current_block_builder <- Some builder

let start_new_block ~ecx =
  let builder = mk_block_builder ~ecx in
  set_block_builder ~ecx builder;
  builder.id

let finish_block ~ecx next =
  let next =
    match ecx.current_block_builder with
    | Some { instructions = (_, Ret _) :: _; _ } -> Block.Halt
    | _ -> next
  in
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

let start_block_sequence ~ecx source =
  ecx.current_block_sequence_ids <- [];
  ecx.current_block_source <- source

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids

let push_loop_context ~ecx break_id continue_id =
  ecx.current_loop_contexts <- (break_id, continue_id) :: ecx.current_loop_contexts

let pop_loop_context ~ecx = ecx.current_loop_contexts <- List.tl ecx.current_loop_contexts

let get_loop_context ~ecx = List.hd ecx.current_loop_contexts

let get_module_builder ~ecx = Option.get ecx.current_module_builder

let start_module ~ecx name =
  ecx.current_module_builder <- Some { Module.name; funcs = SSet.empty; globals = SSet.empty }

let end_module ~ecx =
  let mod_ = get_module_builder ~ecx in
  ecx.modules <- SMap.add mod_.name mod_ ecx.modules;
  ecx.current_module_builder <- None
