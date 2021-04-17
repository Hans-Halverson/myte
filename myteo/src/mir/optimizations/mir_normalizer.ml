open Basic_collections
open Mir
open Mir_visitor
module Ocx = Mir_optimize_context

class var_gatherer ~program =
  object
    inherit IRVisitor.t ~program

    val mutable vars : ISet.t = ISet.empty

    method vars = vars

    method! visit_result_variable ~block:_ ~instruction:_ var_id = vars <- ISet.add var_id vars

    method! visit_phi_node ~block:_ (_, var_id, _) = vars <- ISet.add var_id vars
  end

let normalize ~ocx =
  let open Block in
  (* Gather all vars defined in program *)
  let gatherer = new var_gatherer ~program:ocx.Ocx.program in
  gatherer#run ();
  let vars = gatherer#vars in

  (* Update and potentially prune phi nodes with missing vars *)
  IMap.iter
    (fun _ block ->
      block.phis <-
        List.filter_map
          (fun (value_type, var_id, args) ->
            let args' = IMap.filter (fun _ arg_var_id -> ISet.mem arg_var_id vars) args in
            if IMap.is_empty args' then
              None
            else
              Some (value_type, var_id, args'))
          block.phis)
    ocx.program.blocks;

  (* Find and remove empty blocks *)
  IMap.iter
    (fun block_id block -> if Ocx.can_remove_block ~ocx block then Ocx.remove_block ~ocx block_id)
    ocx.program.blocks;

  (* Remove trivial phis and rewrite references to these phi vars in program, requires iteration
     to a fixpoint *)
  let rewrite_map = ref IMap.empty in
  let rec iter () =
    IMap.iter
      (fun _ block ->
        block.phis <-
          List.filter
            (fun (_, var_id, args) ->
              let (_, arg_var_id) = IMap.choose args in
              if IMap.for_all (fun _ arg -> arg = arg_var_id) args then (
                rewrite_map := IMap.add var_id arg_var_id !rewrite_map;
                false
              ) else
                true)
            block.phis)
      ocx.program.blocks;
    if IMap.is_empty !rewrite_map then
      ()
    else
      let rewrite_mapper = new Mir_mapper.rewrite_vars_mapper ~ocx !rewrite_map in
      let all_blocks = ocx.Ocx.program.blocks in
      IMap.iter
        (fun _ block ->
          let open Block in
          block.phis <- rewrite_mapper#map_phis ~block block.phis;
          block.instructions <- rewrite_mapper#map_instructions ~block block.instructions;
          block.next <- rewrite_mapper#map_block_next ~block block.next)
        all_blocks;
      rewrite_map := IMap.empty;
      iter ()
  in
  iter ();

  (* Consolidate blocks into a single large block when possible, requires iteration to a fixpoint *)
  let removed_blocks = ref ISet.empty in
  let rec iter () =
    IMap.iter
      (fun block_id block ->
        (* Can only consolidate this block if it continues to a block with no other previous blocks,
           and the next block has no phis (as phi arg vars may have been defined in this block). *)
        match block.next with
        | Continue next_block_id
          when block_id <> next_block_id && not (ISet.mem block_id !removed_blocks) ->
          let next_block = Ocx.get_block ~ocx next_block_id in
          (* The next block could be the start block for the global or function, in which case it cannot
             be merged with the previous block. *)
          let next_block_is_start =
            match next_block.source with
            | GlobalInit name ->
              let global = SMap.find name ocx.program.globals in
              global.init_start_block = next_block_id
            | FunctionBody name ->
              let func = SMap.find name ocx.program.funcs in
              func.body_start_block = next_block_id
          in
          let prev_blocks = IMap.find next_block_id ocx.prev_blocks in
          if ISet.cardinal prev_blocks = 1 && next_block.phis = [] && not next_block_is_start then (
            removed_blocks := ISet.add next_block_id !removed_blocks;
            Ocx.merge_adjacent_blocks ~ocx block_id next_block_id
          )
        | _ -> ())
      ocx.program.blocks;
    if ISet.is_empty !removed_blocks then
      ()
    else (
      removed_blocks := ISet.empty;
      iter ()
    )
  in
  iter ()
