open Mir
open Mir_builders

(*
 * For blocks that have multiple previous blocks and multiple next blocks, determine if any of the
 * previous blocks are guaranteed to continue to a single next block. Rewrite this path to continue
 * from the previous block, to a copy of the target block, to the next block.
 *
 * Copying an arbitrary block would require potentitally placing phi nodes across the rest of the
 * program to merge values from the original and copied blocks. We perform a limited version of jump
 * threading that avoids this by only copying the target block if none of its instructions are used
 * outside the block itself and the next blocks that are not part of the threaded path.
 *)

class jump_threading_mapper ~(target_block : Block.t) =
  object
    (* Map contains folded constants and instructions mapped from original block to new block *)
    val mutable value_map : Value.t VMap.t = VMap.empty

    (* The copy of the target block, lazily created *)
    val mutable new_target_block : Block.t option = None

    (* Whether to map instructions in the target block to a new instruction in the copied block.
       We do not create new instructions when constant folding to check for threaded path. *)
    val mutable copy_instructions : bool = false

    method value_map = value_map

    method add_value_mapping v1 v2 = value_map <- VMap.add v1 v2 value_map

    method set_copy_instructions can_copy = copy_instructions <- can_copy

    method map_value value =
      match VMap.find_opt value value_map with
      | Some mapped_value -> mapped_value
      | None when not copy_instructions -> value
      | None ->
        (* Instructions in the target block are copied, all other values are preserved *)
        (match value.value with
        | Instr instr when instr.block == target_block ->
          let mapped_value = mk_uninit_value () in
          (* Temporarily use old value, this will be overwritten but is necessary to pass type checks
             on this value before it is filled. *)
          mapped_value.value <- value.value;
          value_map <- VMap.add value mapped_value value_map;
          mapped_value
        | _ -> value)

    method map_block (block : Block.t) =
      (* Only map the target block to a new block as it will be copied, but other blocks are not *)
      if block == target_block then (
        match new_target_block with
        | Some new_target_block -> new_target_block
        | None ->
          let new_block = mk_block ~func:target_block.func in
          new_target_block <- Some new_block;
          new_block
      ) else
        block
  end

type threaded_path = {
  mapper: jump_threading_mapper;
  threaded_prev_block: Block.t;
  target_block: Block.t;
  threaded_next_block: Block.t;
}

let rec run ~(program : Program.t) =
  program_iter_blocks program (fun target_block ->
      if
        BlockSet.cardinal target_block.prev_blocks > 1
        && BlockSet.cardinal (get_next_blocks target_block) > 1
        && block_has_phis target_block
      then (
        let threaded_paths =
          BlockSet.fold
            (fun prev_block acc ->
              let mapper = new jump_threading_mapper ~target_block in
              get_threaded_path ~mapper prev_block target_block :: acc)
            target_block.prev_blocks
            []
        in
        List.iter (Option.iter rewrite_threaded_path) threaded_paths;

        (* If all paths were threaded then target block has been copied into each rewritten
           threaded path, so the original block can be deleted. *)
        if List.for_all Option.is_some threaded_paths then
          remove_unreachable_blocks_from_root target_block
      ))

(* Determine if the prev block always leads to a specific next block by constant folding block
   with phis mapped to the value coming from the prev block. If found return the threaded path
   along with the mapper containing the constant folded instructions. *)
and get_threaded_path ~mapper (prev_block : Block.t) (target_block : Block.t) =
  iter_instructions target_block (fun instr_value instr ->
      match instr.instr with
      (* Phis are replaced with the value coming from the selected prev block *)
      | Phi phi_instr ->
        let value_from_block = BlockMap.find prev_block phi_instr.args in
        mapper#add_value_mapping instr_value value_from_block.value
      (* All other instructions are constant folded given the phi mappings *)
      | _ ->
        (match Fold_constants.try_fold_instruction_with_mapper ~mapper instr with
        | None -> ()
        | Some constant -> mapper#add_value_mapping instr_value (mk_value (Lit constant))));

  (* Threaded path has been found if branch test is determined to be a boolean constant *)
  match get_terminator target_block with
  | Some { instr = Branch { test; continue; jump }; _ } when VMap.mem test.value mapper#value_map ->
    (match mapper#map_value test.value with
    | { value = Lit (Bool test_value); _ } ->
      let (threaded_next_block, other_next_block) =
        if test_value then
          (continue, jump)
        else
          (jump, continue)
      in

      (* Check if any instruction in this block is used outside the block. If so we cannot copy
         block as we would need to insert phis across the program to merge original and copied
         values before all of their uses. *)
      let instr_used_outside_block = ref false in
      iter_instructions target_block (fun instr_value _ ->
          value_iter_uses ~value:instr_value (fun use ->
              match use.Use.user.value with
              | Instr { block = use_block; _ }
                when use_block != target_block && use_block != other_next_block ->
                instr_used_outside_block := true
              | _ -> ()));

      if !instr_used_outside_block then
        None
      else
        Some { mapper; threaded_prev_block = prev_block; target_block; threaded_next_block }
    | _ -> None)
  | _ -> None

and rewrite_threaded_path threaded_path =
  let { mapper; threaded_prev_block; target_block; threaded_next_block } = threaded_path in

  mapper#set_copy_instructions true;
  let new_target_block = mapper#map_block target_block in

  iter_instructions target_block (fun instr_value instr ->
      match instr.instr with
      | Phi phi -> phi_remove_arg ~phi ~block:threaded_prev_block
      | Branch _ -> ()
      | _ ->
        (* Copy all instructions into new block *)
        (match (mapper#map_value instr_value).value with
        | Lit _
        | Argument _ ->
          ()
        | Instr _ ->
          Mir_mapper.map_instruction ~mapper instr_value;
          let copied_instr = mapper#map_value instr_value in
          append_instruction new_target_block copied_instr));

  (* New block continues to threaded next block *)
  mk_continue_ ~block:new_target_block ~continue:threaded_next_block;
  add_block_link new_target_block threaded_next_block;
  map_phi_backreferences_for_block
    ~block:threaded_next_block
    ~from:target_block
    ~to_:(BlockSet.add target_block (BlockSet.singleton new_target_block));

  (* Previous block continues to new block which contains mapped instructions from original block *)
  map_next_block threaded_prev_block ~from:target_block ~to_:new_target_block
