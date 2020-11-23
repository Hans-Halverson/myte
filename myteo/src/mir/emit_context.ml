open Basic_collections
open Mir

module BlockBuilder = struct
  module Phi = struct
    type t = {
      var_id: var_id;
      old_var_id: var_id option;
      assign_locs: LocSet.t;
    }
  end

  type t = {
    id: Block.id;
    mutable phis: Phi.t list;
    (* Instructions in the block currently being built, in reverse *)
    mutable instructions: (Loc.t * Instruction.t) list;
    mutable next: Block.next;
  }
end

type t = {
  (* Data structures for MIR *)
  main_id: Block.id;
  mutable blocks: BlockBuilder.t IMap.t;
  mutable globals: Global.t LocMap.t;
  mutable funcs: Function.t LocMap.t;
  mutable current_block_builder: BlockBuilder.t;
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
  (* Current IR variables for all program variables in scope *)
  mutable current_var_ids: var_id LocMap.t list;
  (* Updated IR variables that have been captured *)
  mutable current_updated_vars: var_id LocMap.t;
}

let mk () =
  {
    main_id = mk_block_id ();
    blocks = IMap.empty;
    globals = LocMap.empty;
    funcs = LocMap.empty;
    current_block_builder = { id = 0; phis = []; instructions = []; next = Halt };
    current_block_sequence_ids = [];
    current_var_ids = [LocMap.empty];
    current_updated_vars = LocMap.empty;
  }

let builders_to_blocks builders : Block.t IMap.t =
  IMap.map
    (fun builder ->
      {
        Block.id = builder.BlockBuilder.id;
        instructions = List.rev builder.instructions;
        next = builder.next;
      })
    builders

let add_block ~ecx block_builder =
  ecx.blocks <- IMap.add block_builder.BlockBuilder.id block_builder ecx.blocks

let add_global ~ecx global = ecx.globals <- LocMap.add global.Global.loc global ecx.globals

let add_function ~ecx func = ecx.funcs <- LocMap.add func.Function.loc func ecx.funcs

let is_global_loc ~ecx decl_loc = LocMap.mem decl_loc ecx.globals

let emit ~ecx loc inst =
  ecx.current_block_builder.instructions <- (loc, inst) :: ecx.current_block_builder.instructions

let mk_block_builder () =
  { BlockBuilder.id = mk_block_id (); phis = []; instructions = []; next = Halt }

let set_block_builder ~ecx builder = ecx.current_block_builder <- builder

let start_block ~ecx =
  let block_id = mk_block_id () in
  ecx.current_block_builder <- { id = block_id; phis = []; instructions = []; next = Halt };
  block_id

let finish_block ~ecx next =
  let builder = ecx.current_block_builder in
  builder.next <- next;
  add_block ~ecx builder;
  ecx.current_block_sequence_ids <- builder.id :: ecx.current_block_sequence_ids

let finish_block_halt ~ecx = finish_block ~ecx Halt

let start_block_sequence ~ecx = ecx.current_block_sequence_ids <- []

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids

let add_variable ~ecx decl_loc var_id =
  match ecx.current_var_ids with
  | [] -> ()
  | hd :: tl -> ecx.current_var_ids <- LocMap.add decl_loc var_id hd :: tl

let update_variable ~ecx decl_loc var_id =
  let rec update_variable_list var_ids_list =
    match var_ids_list with
    | [] -> []
    | hd :: tl ->
      if LocMap.mem decl_loc hd then
        LocMap.add decl_loc var_id hd :: tl
      else
        hd :: update_variable_list tl
  in
  ecx.current_var_ids <- update_variable_list ecx.current_var_ids;
  ecx.current_updated_vars <- LocMap.add decl_loc var_id ecx.current_updated_vars

let capture_updates ~ecx f =
  let current_var_ids = ecx.current_var_ids in
  ecx.current_updated_vars <- LocMap.empty;
  f ();
  ecx.current_var_ids <- current_var_ids;
  ecx.current_updated_vars

let lookup_variable_in_scope decl_loc scopes =
  let rec lookup lst =
    match lst with
    | [] -> failwith "Declaration must have been added"
    | hd :: tl ->
      (match LocMap.find_opt decl_loc hd with
      | None -> lookup tl
      | Some var_id -> var_id)
  in
  lookup scopes

let lookup_variable ~ecx decl_loc = lookup_variable_in_scope decl_loc ecx.current_var_ids

let enter_variable_scope ~ecx = ecx.current_var_ids <- LocMap.empty :: ecx.current_var_ids

let exit_variable_scope ~ecx = ecx.current_var_ids <- List.tl ecx.current_var_ids

let get_variable_scopes ~ecx = ecx.current_var_ids
