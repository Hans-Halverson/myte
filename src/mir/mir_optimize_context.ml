open Basic_collections
open Mir
open Mir_visitor

type t = {
  (* The program we are optimizing, is internally mutable *)
  program: Program.t;
  (* Block id to the blocks it jumps to *)
  mutable next_blocks: IIMMap.t;
  (* Block id to the previous blocks that jump to it *)
  mutable prev_blocks: IIMMap.t;
}

let get_block ~ocx block_id = IMap.find block_id ocx.program.blocks

let add_block_link ~ocx prev_block next_block =
  ocx.next_blocks <- IIMMap.add prev_block next_block ocx.next_blocks;
  ocx.prev_blocks <- IIMMap.add next_block prev_block ocx.prev_blocks

let remove_block_link ~ocx prev_block next_block =
  ocx.next_blocks <- IIMMap.remove prev_block next_block ocx.next_blocks;
  ocx.prev_blocks <- IIMMap.remove next_block prev_block ocx.prev_blocks

(* Remove all references to a block from phi nodes of on of its next blocks.
   This may be needed when removing a block or block link. *)
let remove_phi_backreferences_for_block ~ocx block_id_to_remove next_block_id =
  let next_block = get_block ~ocx next_block_id in
  block_iter_phis next_block (fun phi ->
      phi.args <- IMap.filter (fun prev_block_id _ -> prev_block_id != block_id_to_remove) phi.args)

(* Replace all references to old_block_id in the phis of a block with new_block_ids. Note that there
   may be multiple new_block_ids, so a single phi argument may be expanded to multiple arguments.
   This may be needed when editing the program. *)
let map_phi_backreferences_for_block ~ocx old_block_id new_block_ids block_to_edit =
  let block = get_block ~ocx block_to_edit in
  block_iter_phis block (fun ({ args; _ } as phi) ->
      match IMap.find_opt old_block_id args with
      | None -> ()
      | Some value ->
        let args_without_old_block_id = IMap.remove old_block_id args in
        phi.args <-
          ISet.fold
            (fun new_block_id args -> IMap.add new_block_id value args)
            new_block_ids
            args_without_old_block_id)

(* An empty block can be removed only if it continues to a single block, and is not needed by any
   phi nodes in its succeeding block. *)
let can_remove_block ~ocx (block : Block.t) =
  block.instructions = []
  &&
  match block.next with
  | Halt
  | Branch _ ->
    false
  | Continue continue_id ->
    (* A block is needed if any of its previous blocks appear in a phi node of the next block, with
       a different value than the value from this block. A block is also needed if it is the start
       block and the next block has any phi nodes. If we were to remove this block, the value from
       its branch would be lost in the phi node. *)
    let continue_block = get_block ~ocx continue_id in
    let prev_nodes = IIMMap.find_all block.id ocx.prev_blocks in

    let func = SMap.find block.func ocx.program.funcs in
    let is_start_block = func.body_start_block = block.id in

    let continue_block_phis = block_get_phis continue_block in
    let block_needed_for_phi =
      (continue_block_phis <> [] && is_start_block)
      || List.exists
           (fun { Instruction.Phi.args; _ } ->
             IMap.exists
               (fun prev_block_id prev_block_arg ->
                 if ISet.mem prev_block_id prev_nodes then
                   not (values_equal prev_block_arg (IMap.find block.id args))
                 else
                   false)
               args)
           continue_block_phis
    in
    let function_start_self_loop = is_start_block && continue_id = block.id in
    (not block_needed_for_phi) && not function_start_self_loop

let remove_block ~ocx block_id =
  let block = get_block ~ocx block_id in
  (* This may be the first block in a function. If so, update the function to point to the
     next block as the start. *)
  (match block.next with
  | Continue continue_block ->
    let func = SMap.find block.func ocx.program.funcs in
    if func.body_start_block = block_id then func.body_start_block <- continue_block
  | _ -> ());
  let prev_blocks = IIMMap.find_all block_id ocx.prev_blocks in

  (match block.next with
  | Continue next_block_id ->
    (* Update phis in next block to reference previous blocks instead of removed block *)
    map_phi_backreferences_for_block ~ocx block_id prev_blocks next_block_id;

    (* Rewrite next of previous blocks to point to next block instead of removed block *)
    ISet.iter
      (fun prev_block_id ->
        let prev_block = get_block ~ocx prev_block_id in
        let map_id id =
          if id = block_id then (
            add_block_link ~ocx prev_block_id next_block_id;
            next_block_id
          ) else
            id
        in
        prev_block.next <-
          (match prev_block.next with
          | Halt -> Halt
          | Continue id -> Continue (map_id id)
          | Branch { test; continue; jump } ->
            let new_continue = map_id continue in
            let new_jump = map_id jump in
            (* If both branches points to same label convert to continue *)
            if new_continue = new_jump then
              Continue new_continue
            else
              (* Otherwise create branch to new block *)
              Branch { test; continue = new_continue; jump = new_jump }))
      prev_blocks
  | Branch _
  | Halt ->
    ());

  (* Remove references to this removed block from phi nodes of next blocks *)
  let next_blocks = IMap.find block_id ocx.next_blocks in
  ISet.iter
    (fun next_block_id -> remove_phi_backreferences_for_block ~ocx block_id next_block_id)
    next_blocks;
  (* Remove prev pointers from next blocks to this removed block *)
  let next_blocks = IMap.find block_id ocx.next_blocks in
  ISet.iter
    (fun next_block_id -> ocx.prev_blocks <- IIMMap.remove next_block_id block_id ocx.prev_blocks)
    next_blocks;
  (* Remove next pointers from prev blocks to this removed block *)
  ISet.iter
    (fun prev_block_id -> ocx.next_blocks <- IIMMap.remove prev_block_id block_id ocx.next_blocks)
    prev_blocks;
  (* Remove block from remaining maps in context *)
  ocx.prev_blocks <- IMap.remove block_id ocx.prev_blocks;
  ocx.next_blocks <- IMap.remove block_id ocx.next_blocks;
  ocx.program.blocks <- IMap.remove block_id ocx.program.blocks

(* Merge adjacent blocks b1 and b2. Must only be called if b1 and b2 can be merged, meaning
   b1 only continues to b2 and b2 has no other previous blocks. *)
let merge_adjacent_blocks ~ocx block_id1 block_id2 =
  let open Block in
  let b1 = get_block ~ocx block_id1 in
  let b2 = get_block ~ocx block_id2 in
  let map_id id =
    if id = block_id2 then
      block_id1
    else
      id
  in
  b1.instructions <- b1.instructions @ b2.instructions;
  (* Use b2's next, but take care to reference b1 instead of b2 in the case of self references *)
  b1.next <-
    (match b2.next with
    | Halt -> Halt
    | Continue continue -> Continue (map_id continue)
    | Branch ({ continue; jump; _ } as branch) ->
      Branch { branch with continue = map_id continue; jump = map_id jump });
  (* References to the b2 block in phi nodes of blocks that succeed b2 should be rewritten
     to now reference b1 instead. *)
  let next_blocks = IIMMap.find_all b2.id ocx.next_blocks in
  ISet.iter
    (fun next_block_id ->
      map_phi_backreferences_for_block ~ocx b2.id (ISet.singleton b1.id) next_block_id)
    next_blocks;
  (* Set prev pointers for blocks that succeed b2 to point to b1 instead *)
  ISet.iter
    (fun next_block_id ->
      remove_block_link ~ocx b2.id next_block_id;
      add_block_link ~ocx b1.id next_block_id)
    next_blocks;
  remove_block_link ~ocx block_id1 block_id2;
  (* Remove b2 from remaining maps in context *)
  ocx.next_blocks <- IMap.remove b2.id ocx.next_blocks;
  ocx.prev_blocks <- IMap.remove b2.id ocx.prev_blocks;
  ocx.program.blocks <- IMap.remove b2.id ocx.program.blocks

class init_visitor ~ocx =
  object
    inherit IRVisitor.t ~program:ocx.program

    method! visit_edge b1 b2 = add_block_link ~ocx b1.id b2.id
  end

let mk program =
  (* Initialize prev and next block maps *)
  let init_block_mmaps =
    IMap.fold
      (fun block_id _ mmap -> IMap.add block_id ISet.empty mmap)
      program.Program.blocks
      IMap.empty
  in
  let ocx = { program; next_blocks = init_block_mmaps; prev_blocks = init_block_mmaps } in
  let init_visitor = new init_visitor ~ocx in
  init_visitor#run ();
  ocx
