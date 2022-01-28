open Mir
open Mir_builders

let add_block_link (prev_block : Block.t) (next_block : Block.t) =
  next_block.prev_blocks <- BlockSet.add prev_block next_block.prev_blocks

let remove_block_link (prev_block : Block.t) (next_block : Block.t) =
  next_block.prev_blocks <- BlockSet.remove prev_block next_block.prev_blocks

(* Remove all references to a block from phi nodes of on of its next blocks.
   This may be needed when removing a block or block link. *)
let remove_phi_backreferences_for_block block_to_remove next_block =
  block_iter_phis next_block (fun phi ->
      phi.args <- BlockMap.filter (fun prev_block _ -> prev_block != block_to_remove) phi.args)

(* Replace all references to old_block_id in the phis of a block with new_block_ids. Note that there
   may be multiple new_block_ids, so a single phi argument may be expanded to multiple arguments.
   This may be needed when editing the program. *)
let map_phi_backreferences_for_block old_block new_blocks block_to_edit =
  block_iter_phis block_to_edit (fun ({ args; _ } as phi) ->
      match BlockMap.find_opt old_block args with
      | None -> ()
      | Some value ->
        let args_without_old_block = BlockMap.remove old_block args in
        phi.args <-
          BlockSet.fold
            (fun new_block args -> BlockMap.add new_block value args)
            new_blocks
            args_without_old_block)

(* An empty block can be removed only if it continues to a single block, and is not needed by any
   phi nodes in its succeeding block. *)
let can_remove_block (block : Block.t) =
  has_single_instruction block
  &&
  match get_terminator block with
  | Some { instr = Continue continue_block; _ } ->
    (* A block is needed if any of its previous blocks appear in a phi node of the next block, with
       a different value than the value from this block. A block is also needed if it is the start
       block and the next block has any phi nodes. If we were to remove this block, the value from
       its branch would be lost in the phi node. *)
    let is_start_block = block.func.start_block == block in

    let continue_block_phis = block_get_phis continue_block in
    let block_needed_for_phi =
      (continue_block_phis <> [] && is_start_block)
      || List.exists
           (fun { Instruction.Phi.args; _ } ->
             BlockMap.exists
               (fun prev_block prev_block_arg ->
                 if BlockSet.mem prev_block block.prev_blocks then
                   not (values_equal prev_block_arg (BlockMap.find block args))
                 else
                   false)
               args)
           continue_block_phis
    in
    let function_start_self_loop = is_start_block && continue_block == block in
    (not block_needed_for_phi) && not function_start_self_loop
  | _ -> false

let remove_block (block : Block.t) =
  (* This may be the first block in a function. If so, update the function to point to the
     next block as the start. *)
  let func = block.func in
  (match get_terminator block with
  | Some { instr = Continue continue_block; _ } ->
    if func.start_block == block then func.start_block <- continue_block
  | _ -> ());
  (match get_terminator block with
  (* Only when removing unreachable blocks from branch pruning, which could include return block.
     Remove any instances of block from previous blocks. *)
  | Some { instr = Unreachable; _ } ->
    BlockSet.iter
      (fun prev_block ->
        match get_terminator prev_block with
        | Some ({ instr = Continue _; _ } as term_instr) -> term_instr.instr <- Unreachable
        | Some ({ instr = Branch { test = _; continue; jump }; _ } as term_instr) ->
          term_instr.instr <-
            ( if continue == block then
              if jump == block then
                Unreachable
              else
                Continue jump
            else
              Continue continue )
        | _ -> failwith "Previous block must have branching terminator")
      block.prev_blocks
  | Some { instr = Continue next_block; _ } ->
    (* Update phis in next block to reference previous blocks instead of removed block *)
    map_phi_backreferences_for_block block block.prev_blocks next_block;

    (* Rewrite next of previous blocks to point to next block instead of removed block *)
    BlockSet.iter
      (fun prev_block -> map_next_block prev_block ~from:block ~to_:next_block)
      block.prev_blocks
  | _ -> ());

  (* Remove references to this removed block from phi nodes of next blocks *)
  let next_blocks = get_next_blocks block in
  BlockSet.iter (fun next_block -> remove_phi_backreferences_for_block block next_block) next_blocks;
  (* Remove prev pointers from next blocks to this removed block *)
  let next_blocks = get_next_blocks block in
  BlockSet.iter
    (fun next_block -> next_block.prev_blocks <- BlockSet.remove block next_block.prev_blocks)
    next_blocks;
  (* Remove block from remaining maps in context *)
  func.blocks <- BlockSet.remove block func.blocks

(* Merge adjacent blocks b1 and b2. Must only be called if b1 and b2 can be merged, meaning
   b1 only continues to b2 and b2 has no other previous blocks. *)
let merge_adjacent_blocks block1 block2 =
  let open Block in
  let map_block block =
    if block == block2 then
      block1
    else
      block
  in
  (* Use b2's next, but take care to reference b1 instead of b2 in the case of self references *)
  (match get_terminator block2 with
  | Some ({ instr = Continue continue; _ } as term_instr) ->
    term_instr.instr <- Continue (map_block continue)
  | Some ({ instr = Branch { test; continue; jump }; _ } as term_instr) ->
    term_instr.instr <- Branch { test; continue = map_block continue; jump = map_block jump }
  | _ -> ());
  concat_instructions block1 block2;
  (* References to the b2 block in phi nodes of blocks that succeed b2 should be rewritten
     to now reference b1 instead. *)
  let next_blocks = get_next_blocks block2 in
  BlockSet.iter
    (fun next_block ->
      map_phi_backreferences_for_block block2 (BlockSet.singleton block1) next_block)
    next_blocks;
  (* Set prev pointers for blocks that succeed b2 to point to b1 instead *)
  BlockSet.iter
    (fun next_block ->
      remove_block_link block2 next_block;
      add_block_link block1 next_block)
    next_blocks;
  remove_block_link block1 block2;
  (* Remove b2 from remaining maps in context *)
  let func = block2.func in
  func.blocks <- BlockSet.remove block2 func.blocks

(* Split an edge between two blocks, inserting an empty block in the middle *)
let split_block_edge (prev_block : Block.t) (next_block : Block.t) : Block.t =
  let func = prev_block.func in
  let new_block =
    {
      Block.id = Block.mk_id ();
      func;
      instructions = None;
      prev_blocks = BlockSet.singleton prev_block;
    }
  in
  ignore (mk_continue ~block:new_block ~continue:next_block);
  func.blocks <- BlockSet.add new_block func.blocks;
  map_next_block prev_block ~from:next_block ~to_:new_block;
  new_block
