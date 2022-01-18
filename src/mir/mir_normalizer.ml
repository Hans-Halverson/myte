open Basic_collections
open Mir
open Mir_visitor
module Ocx = Mir_optimize_context

class var_gatherer ~program =
  object
    inherit IRVisitor.t ~program as super

    val mutable value_ids : ISet.t = ISet.empty

    method value_ids = value_ids

    method! visit_function func =
      List.iter (fun param -> value_ids <- ISet.add param.Argument.id value_ids) func.params;
      super#visit_function func

    method! visit_instruction instr = value_ids <- ISet.add instr.id value_ids
  end

let rec normalize ~ocx =
  (* Gather all vars defined in program *)
  let gatherer = new var_gatherer ~program:ocx.Ocx.program in
  gatherer#run ();
  let value_ids = gatherer#value_ids in

  (* Update and potentially prune phi nodes with missing vars *)
  IMap.iter
    (fun _ block ->
      block_filter_phis block (fun _ ({ args } as phi) ->
          let args' =
            IMap.filter
              (fun _ arg_val ->
                match arg_val with
                | Value.Lit _ -> true
                | Argument { id; _ }
                | Instr { id; _ } ->
                  ISet.mem id value_ids)
              args
          in
          let has_args = not (IMap.is_empty args') in
          if has_args then phi.args <- args';
          has_args))
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
        block_filter_phis block (fun instr_id { args } ->
            let (_, arg_val) = IMap.choose args in
            if IMap.for_all (fun _ arg -> values_equal arg arg_val) args then (
              rewrite_map := IMap.add instr_id arg_val !rewrite_map;
              false
            ) else
              true))
      ocx.program.blocks;
    if IMap.is_empty !rewrite_map then
      ()
    else
      let rewrite_mapper = new Mir_mapper.rewrite_vals_mapper ~program:ocx.program !rewrite_map in
      let all_blocks = ocx.Ocx.program.blocks in
      IMap.iter (fun _ block -> rewrite_mapper#map_block block) all_blocks;
      rewrite_map := IMap.empty;
      iter ()
  in
  iter ();

  consolidate_adjacent_blocks ~ocx;
  remove_empty_init_func ~ocx

(* Consolidate adjacent blocks into a single large block when possible *)
and consolidate_adjacent_blocks ~ocx =
  let removed_blocks = ref ISet.empty in
  (* Iterate to fixpoint *)
  let rec iter () =
    IMap.iter
      (fun block_id block ->
        let open Block in
        (* Can only consolidate this block if it continues to a block with no other previous blocks,
           and the next block has no phis (as phi arg vars may have been defined in this block). *)
        match block.next with
        | Continue next_block_id
          when block_id <> next_block_id && not (ISet.mem block_id !removed_blocks) ->
          let next_block = Ocx.get_block ~ocx next_block_id in
          (* The next block could be the start block for the global or function, in which case it cannot
             be merged with the previous block. *)
          let next_block_is_start =
            let func = SMap.find next_block.func ocx.program.funcs in
            func.body_start_block = next_block_id
          in
          let prev_blocks = IMap.find next_block_id ocx.prev_blocks in
          if
            ISet.cardinal prev_blocks = 1
            && (not (block_has_phis next_block))
            && not next_block_is_start
          then (
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

(* Strip init function if it is empty *)
and remove_empty_init_func ~ocx =
  match SMap.find_opt init_func_name ocx.program.funcs with
  | None -> ()
  | Some init_func ->
    (* Init function is empty if it consists of a single block with a single instruction (Ret) *)
    let init_start_block = IMap.find init_func.body_start_block ocx.program.blocks in
    if has_single_instruction init_start_block && init_start_block.next = Halt then
      ocx.program.funcs <- SMap.remove init_func_name ocx.program.funcs
