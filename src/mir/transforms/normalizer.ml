open Basic_collections
open Mir
open Mir_builders

(*
  Simplify the MIR, applying the following changes:
   - Replace all phis where all arguments are the same value (including single argument phis) with
     that argument.
   - Remove empty blocks
   - Merge adjacent blocks when possible
   - Remove the init function if it is empty
*)

let rec run ~program =
  simplify_phis ~program;
  remove_empty_blocks ~program;
  merge_adjacent_blocks ~program;
  remove_empty_init_func ~program

(* Merge adjacent blocks into a single large block when possible *)
and merge_adjacent_blocks ~program =
  let removed_blocks = ref BlockSet.empty in
  (* Iterate to fixpoint *)
  let rec iter () =
    program_iter_blocks program (fun (block : Block.t) ->
        (* Can only merge this block if it continues to a block with no other previous blocks,
           and the next block has no phis (as phi arg vars may have been defined in this block). *)
        match get_terminator block with
        | Some { instr = Continue next_block; _ }
          when block != next_block && not (BlockSet.mem block !removed_blocks) ->
          let next_block_is_start = next_block.func.start_block == next_block in
          (* The next block could be the start block for the global or function, in which case it cannot
             be merged with the previous block. *)
          if
            BlockSet.cardinal next_block.prev_blocks = 1
            && (not (block_has_phis next_block))
            && not next_block_is_start
          then (
            removed_blocks := BlockSet.add next_block !removed_blocks;
            Mir_builders.merge_adjacent_blocks block next_block
          )
        | _ -> ());
    if BlockSet.is_empty !removed_blocks then
      ()
    else (
      removed_blocks := BlockSet.empty;
      iter ()
    )
  in
  iter ()

(* Strip init function if it is empty *)
and remove_empty_init_func ~program =
  match SMap.find_opt init_func_name program.funcs with
  | None -> ()
  | Some init_func ->
    (* Init function is empty if it consists of a single block with a single instruction (Ret) *)
    let init_start_block = init_func.start_block in
    let is_ret_terminator =
      match get_terminator init_start_block with
      | Some { instr = Ret _; _ } -> true
      | _ -> false
    in
    if is_ret_terminator && has_single_instruction init_start_block then
      program.funcs <- SMap.remove init_func_name program.funcs

and remove_empty_blocks ~program = program_iter_blocks program block_remove_if_empty

(* Replace phis where all args have same value (including single arg phis) with the arg itself *)
and simplify_phi ~worklist (phi_instr : Value.t) =
  let phi = cast_to_phi (cast_to_instruction phi_instr) in
  match phi_get_single_arg_value phi with
  | None -> ()
  | Some arg_value ->
    (* If replaced phi is as arg of other phis, recheck those phis *)
    value_iter_uses ~value:phi_instr (fun use ->
        match use.value.value with
        | Instr { instr = Phi _; _ } -> worklist := VSet.add use.value !worklist
        | _ -> ());
    replace_instruction ~from:phi_instr ~to_:arg_value

and simplify_phis ~program =
  let worklist = ref VSet.empty in
  (* Initial pass visits all instructions, enqueuing dependent uses to recheck *)
  program_iter_blocks program (fun block ->
      block_iter_phis block (fun phi_instr _ -> simplify_phi ~worklist phi_instr));
  (* Keep checking for simplification and enqueuing depdent uses until no possible changes are left *)
  while not (VSet.is_empty !worklist) do
    let instr_value = VSet.choose !worklist in
    worklist := VSet.remove instr_value !worklist;
    simplify_phi ~worklist instr_value
  done
