open Basic_collections
open Mir
open Mir_builders
open Mir_visitor

class var_gatherer ~program =
  object
    inherit IRVisitor.t ~program as super

    val mutable value_ids : ISet.t = ISet.empty

    method value_ids = value_ids

    method! visit_function func =
      List.iter
        (fun param ->
          let arg = cast_to_argument param in
          value_ids <- ISet.add arg.id value_ids)
        func.params;
      super#visit_function func

    method! visit_instruction _ instr = value_ids <- ISet.add instr.id value_ids
  end

let rec normalize ~program =
  (* Find and remove empty blocks *)
  program_iter_blocks program (fun block -> if can_remove_block block then remove_block block);

  (* Remove trivial phis and rewrite references to these phi vars in program, requires iteration
     to a fixpoint *)
  (* let rewrite_map = ref IMap.empty in
     let rec iter () =
       program_iter_blocks program (fun block ->
           block_filter_phis block (fun instr_id { args } ->
               let (_, chosen_use) = BlockMap.choose args in
               if
                 BlockMap.for_all
                   (fun _ arg_use -> values_equal arg_use.Use.value chosen_use.value)
                   args
               then (
                 rewrite_map := IMap.add instr_id chosen_use.value !rewrite_map;
                 false
               ) else
                 true));
       if IMap.is_empty !rewrite_map then
         ()
       else
         let rewrite_mapper = new Mir_mapper.rewrite_vals_mapper ~program !rewrite_map in
         program_iter_blocks program rewrite_mapper#map_block;
         rewrite_map := IMap.empty;
         iter ()
     in
     iter (); *)
  consolidate_adjacent_blocks ~program;
  remove_empty_init_func ~program

(* Consolidate adjacent blocks into a single large block when possible *)
and consolidate_adjacent_blocks ~program =
  let removed_blocks = ref BlockSet.empty in
  (* Iterate to fixpoint *)
  let rec iter () =
    program_iter_blocks program (fun (block : Block.t) ->
        (* Can only consolidate this block if it continues to a block with no other previous blocks,
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
            merge_adjacent_blocks block next_block
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
