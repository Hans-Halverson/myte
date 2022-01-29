open Basic_collections
open Mir
open Mir_builders

(* Return the set of all blocks that this block branches to in a logical order *)
let get_ordered_next_blocks (block : Block.t) : Block.t list =
  match get_terminator block with
  | Some { instr = Continue continue; _ } -> [continue]
  | Some { instr = Branch { test = _; jump; continue }; _ } -> [continue; jump]
  | _ -> []

(* Run Tarjan's strongly connected components algorithm on a set of blocks. Return a pair containing
   a set of all SCCs, as well as a map from block to the SCC it is a part of. *)
let tarjans_scc blocks =
  let max_scc_id = ref 0 in
  let block_to_scc_id = ref BlockMap.empty in
  let sccs = ref ISet.empty in

  let index = ref 0 in
  let block_index = ref BlockMap.empty in
  let block_lowlink = ref BlockMap.empty in
  let blocks_on_stack = ref BlockSet.empty in
  let stack = Stack.create () in

  let is_visited block = BlockMap.mem block !block_index in
  let index_of block = BlockMap.find block !block_index in
  let lowlink_of block = BlockMap.find block !block_lowlink in
  let set_lowlink block lowlink = block_lowlink := BlockMap.add block lowlink !block_lowlink in

  let rec strong_connect block =
    block_index := BlockMap.add block !index !block_index;
    set_lowlink block !index;
    index := !index + 1;
    Stack.push block stack;
    blocks_on_stack := BlockSet.add block !blocks_on_stack;

    (* Get next blocks manually to preserve order *)
    let next_blocks = get_next_blocks block in
    BlockSet.iter
      (fun (next_block : Block.t) ->
        if not (is_visited next_block) then (
          strong_connect next_block;
          set_lowlink block (min (lowlink_of block) (lowlink_of next_block))
        ) else if BlockSet.mem next_block !blocks_on_stack then
          set_lowlink block (min (lowlink_of block) (index_of next_block)))
      next_blocks;

    (* Generate strongly connected component by popping off stack *)
    if lowlink_of block = index_of block then (
      let scc_id = !max_scc_id in
      max_scc_id := !max_scc_id + 1;

      let rec collect_scc () =
        let next_block = Stack.pop stack in
        blocks_on_stack := BlockSet.remove next_block !blocks_on_stack;
        block_to_scc_id := BlockMap.add next_block scc_id !block_to_scc_id;
        if next_block != block then collect_scc ()
      in
      collect_scc ();
      sccs := ISet.add scc_id !sccs
    )
  in

  BlockSet.iter (fun block -> if not (is_visited block) then strong_connect block) blocks;
  (sccs, block_to_scc_id)

(* Order all blocks in the program that are reachable from the provided starting block. We attempt
   to generate an ordering that resembles the original source program and keeps adjacent blocks near
   each other.

   In general we would like to order via a depth-first topological sort when possible. This is
   complicated by the fact that the control flow graph (CFG) may contain cycles. To account for this
   we first compute the strongly connected components (SCCs) of the CFG, which will form a DAG,
   and then visit them in depth-first topological order.

   Within each SCC we approximate a topological sort to the best of our ability. We keep track of
   the incoming degree for each block, and greedily visit blocks (in a depth-first traversal) if all
   their predecessors have been visited. Otherwise, if a block is in the current SCC but all its
   predecessors have not been visited we enqueue it in a frontier for the current SCC, and pop
   blocks off this queue to visit whenever we reach a dead end.

   If the entire current SCC has been visited (known because we reach a dead end and there are no
   blocks currently on the current SCC frontier), then we visit the next SCC in depth-first
   topological order.
 *)
let order_blocks start_block =
  (* Depth-first traversal to gather all blocks *)
  let all_blocks = ref BlockSet.empty in
  let rec visit_block block =
    if not (BlockSet.mem block !all_blocks) then begin
      all_blocks := BlockSet.add block !all_blocks;
      let next_blocks = get_next_blocks block in
      BlockSet.iter visit_block next_blocks
    end
  in
  visit_block start_block;

  (* Run Tarjan's algorithm to find strongly connected components of control flow graph *)
  let (sccs, block_to_scc_id) = tarjans_scc !all_blocks in

  (* Build map of predecessor degree for each block, and map from SCC id the set of SCCs that
     precede it in the SCC DAG. *)
  let scc_preds = ref IMap.empty in
  let block_pred_degree = ref BlockMap.empty in
  let add_scc_pred pred_scc_id (succ_block : Block.t) =
    let succ_scc_id = BlockMap.find succ_block !block_to_scc_id in
    if pred_scc_id != succ_scc_id then
      scc_preds :=
        IMap.add succ_scc_id (ISet.add pred_scc_id (IMap.find succ_scc_id !scc_preds)) !scc_preds
  in
  let remove_scc_pred pred_scc_id (succ_block : Block.t) =
    let succ_scc_id = BlockMap.find succ_block !block_to_scc_id in
    if pred_scc_id != succ_scc_id then
      scc_preds :=
        IMap.add succ_scc_id (ISet.remove pred_scc_id (IMap.find succ_scc_id !scc_preds)) !scc_preds
  in
  let inc_block_pred (block : Block.t) =
    let old_degree = BlockMap.find block !block_pred_degree in
    block_pred_degree := BlockMap.add block (old_degree + 1) !block_pred_degree
  in
  let dec_block_pred (block : Block.t) =
    block_pred_degree :=
      BlockMap.add block (BlockMap.find block !block_pred_degree - 1) !block_pred_degree
  in
  ISet.iter (fun scc_id -> scc_preds := IMap.add scc_id ISet.empty !scc_preds) !sccs;
  BlockSet.iter
    (fun block -> block_pred_degree := BlockMap.add block 0 !block_pred_degree)
    !all_blocks;
  BlockSet.iter
    (fun block ->
      let scc_id = BlockMap.find block !block_to_scc_id in
      let next_blocks = get_next_blocks block in
      BlockSet.iter
        (fun next_block ->
          inc_block_pred next_block;
          add_scc_pred scc_id next_block)
        next_blocks)
    !all_blocks;

  let blocks = ref [] in
  let visited_blocks = ref BlockSet.empty in
  let is_visited block = BlockSet.mem block !visited_blocks in

  (* Blocks in other SCCs that are ready to be visited (meaning all predecessor SCCs have already
     been visited). This is represented as a stack of queues, where each queue contains the blocks
     in other SCCs gathered during the traversal of a single SCC. A stack of queues is used to give
     an in-order traversal (the same traversal order as recursive DFS). *)
  let next_sccs_frontier = Stack.create () in
  (* Blocks within the current SCC that are ready to be visited (meaning all predecessor blocks have
     already been visited). *)
  let within_scc_frontier = Queue.create () in

  let rec iter ~prev_scc () =
    (* First try to visit a block within the current SCC *)
    match Queue.take_opt within_scc_frontier with
    | Some next_block ->
      if not (is_visited next_block) then visit_block ~scc_id:prev_scc next_block;
      iter ~prev_scc ()
    | None ->
      (* Otherwise try to visit a new SCC *)
      (match Stack.top_opt next_sccs_frontier with
      | None -> ()
      | Some queue ->
        (match Queue.take_opt queue with
        | None ->
          ignore (Stack.pop next_sccs_frontier);
          iter ~prev_scc ()
        | Some next_block ->
          (* If we are entering a new SCC, add a new queue to the stack to keep track of the next
             SCCs to visit from the new SCC. *)
          let next_scc = BlockMap.find next_block !block_to_scc_id in
          if prev_scc != next_scc then Stack.push (Queue.create ()) next_sccs_frontier;
          if not (is_visited next_block) then visit_block ~scc_id:next_scc next_block;
          iter ~prev_scc:next_scc ()))
  and visit_block ~scc_id block =
    visited_blocks := BlockSet.add block !visited_blocks;
    blocks := block :: !blocks;
    let ordered_next_blocks = get_ordered_next_blocks block in
    List.iter
      (fun next_block ->
        dec_block_pred next_block;
        remove_scc_pred scc_id next_block;
        visit_or_enqueue ~prev_scc_id:scc_id next_block)
      ordered_next_blocks
  and visit_or_enqueue ~prev_scc_id next_block =
    let next_scc_id = BlockMap.find next_block !block_to_scc_id in
    if is_visited next_block then
      ()
    else if prev_scc_id = next_scc_id then
      (* If the next block is within the same SCC, visit it immediately if all its predecessors
         have been visited, otherwise enqueue in within the current SCC's frontier. *)
      if BlockMap.find next_block !block_pred_degree = 0 then
        visit_block ~scc_id:prev_scc_id next_block
      else
        Queue.push next_block within_scc_frontier
    else if ISet.cardinal (IMap.find next_scc_id !scc_preds) = 0 then
      (* If the next block is within another SCC whose predecessor SCCs have been visited then
         enqueue it in the frontier for new SCCs to visit *)
      Queue.push next_block (Stack.top next_sccs_frontier)
  in

  (* Start with a stack of a single queue containing the start block id *)
  Stack.push (Queue.create ()) next_sccs_frontier;
  Queue.push start_block (Stack.top next_sccs_frontier);

  iter ~prev_scc:(BlockMap.find start_block !block_to_scc_id) ();
  List.rev !blocks
