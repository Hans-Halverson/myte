open Basic_collections
open Mir

(* SSA destruction - remove all phi nodes in program and replace with explicit move instructions.
   Moves are inserted in the previous block, unless the previous block branches, in which case the
   edge is split and moves are inserted in a new block between the previous and current blocks.
 *)
let destruct_ssa (ir : Program.t) : Program.t =
  let open Block in
  let edges_to_split = ref IMap.empty in
  let mark_edge_to_split prev_block block_id =
    let edges =
      match IMap.find_opt block_id !edges_to_split with
      | None -> ISet.singleton prev_block
      | Some edges -> ISet.add prev_block edges
    in
    edges_to_split := IMap.add block_id edges !edges_to_split
  in

  (* Mark all edges that should be split. An edge from block A to block B should be split if
     block B contains a phi node that references A, but block A branches at the end. *)
  IMap.iter
    (fun _ block ->
      List.iter
        (fun (_, _, args) ->
          IMap.iter
            (fun prev_block_id _ ->
              let prev_block = IMap.find prev_block_id ir.blocks in
              match prev_block.next with
              | Branch _ -> mark_edge_to_split prev_block_id block.id
              | _ -> ())
            args)
        block.phis)
    ir.blocks;

  (* Split all marked edges *)
  IMap.iter
    (fun block_id prev_blocks ->
      let block = IMap.find block_id ir.blocks in
      (* Create and insert new blocks, keeping map from previous block to new inserted block *)
      let prev_to_new_block =
        ISet.fold
          (fun prev_block_id prev_to_new_block ->
            let prev_block = IMap.find prev_block_id ir.blocks in
            match prev_block.next with
            | Branch { test; continue; jump } ->
              (* Create new empty block and insert between previous block and this block *)
              let new_block_id = mk_block_id () in
              let new_block =
                {
                  Block.id = new_block_id;
                  func = block.func;
                  instructions = [];
                  phis = [];
                  next = Continue block.id;
                }
              in
              ir.blocks <- IMap.add new_block_id new_block ir.blocks;
              let map_next_block next_block =
                if next_block = block.id then
                  new_block_id
                else
                  next_block
              in
              prev_block.next <-
                Branch { test; continue = map_next_block continue; jump = map_next_block jump };
              IMap.add prev_block_id new_block_id prev_to_new_block
            | _ -> failwith "Only blocks which branch need to be split")
          prev_blocks
          IMap.empty
      in
      (* Rewrite references to previous block to instead reference newly inserted block *)
      block.phis <-
        List.map
          (fun (value_type, var_id, args) ->
            let args' =
              IMap.fold
                (fun prev_block_id arg args' ->
                  match IMap.find_opt prev_block_id prev_to_new_block with
                  | None -> IMap.add prev_block_id arg args'
                  | Some new_block_id -> IMap.add new_block_id arg args')
                args
                IMap.empty
            in
            (value_type, var_id, args'))
          block.phis)
    !edges_to_split;

  (* Remove phis, and replace with explicit move instructions in the previous block *)
  IMap.iter
    (fun _ block ->
      List.iter
        (fun (_, var_id, args) ->
          IMap.iter
            (fun prev_block_id arg_val ->
              let mov_instruction = (mk_instr_id (), Instruction.Mov (var_id, arg_val)) in
              let prev_block = IMap.find prev_block_id ir.blocks in
              match prev_block.next with
              | Continue _ -> prev_block.instructions <- prev_block.instructions @ [mov_instruction]
              | _ -> failwith "Moves can only be inserted if previous block does not branch")
            args)
        block.phis;
      block.phis <- [])
    ir.blocks;
  ir
