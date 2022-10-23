open Basic_collections
open Mir
open Mir_builders

module type NODE_TYPE = sig
  type t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  val compare : t -> t -> int

  val iter_next : t -> (t -> unit) -> unit
end

(* Traverse a graph that may contain cycles in a topological order to the best of our ability.
   Strongly connected components within the graph will be visited in topological order, and within
   strongly connected components we do our best to visit predecessors before successors.

   This is used for ordering various graphs, such as the CFG within a particular function or the
   function call graph over the entire program. *)
module Make (Node : NODE_TYPE) = struct
  module Map = Node.Map
  module Set = Node.Set

  module MMap = MultiMap.Make (Node.Map) (Node.Set)

  (* Run Tarjan's strongly connected components algorithm on a graph. Return a pair containing
     a set of all SCCs, as well as a map from node to the SCC it is a part of. *)
  let tarjans_scc nodes =
    let max_scc_id = ref 0 in
    let node_to_scc_id = ref Map.empty in
    let sccs = ref ISet.empty in

    let index = ref 0 in
    let node_index = ref Map.empty in
    let node_lowlink = ref Map.empty in
    let nodes_on_stack = ref Set.empty in
    let stack = Stack.create () in

    let is_visited node = Map.mem node !node_index in
    let index_of node = Map.find node !node_index in
    let lowlink_of node = Map.find node !node_lowlink in
    let set_lowlink node lowlink = node_lowlink := Map.add node lowlink !node_lowlink in

    let rec strong_connect node =
      node_index := Map.add node !index !node_index;
      set_lowlink node !index;
      index := !index + 1;
      Stack.push node stack;
      nodes_on_stack := Set.add node !nodes_on_stack;

      (* Visit succeeding nodes *)
      Node.iter_next node (fun next_node ->
          if not (is_visited next_node) then (
            strong_connect next_node;
            set_lowlink node (min (lowlink_of node) (lowlink_of next_node))
          ) else if Set.mem next_node !nodes_on_stack then
            set_lowlink node (min (lowlink_of node) (index_of next_node)));

      (* Generate strongly connected component by popping off stack *)
      if lowlink_of node = index_of node then (
        let scc_id = !max_scc_id in
        max_scc_id := !max_scc_id + 1;

        let rec collect_scc () =
          let next_node = Stack.pop stack in
          nodes_on_stack := Set.remove next_node !nodes_on_stack;
          node_to_scc_id := Map.add next_node scc_id !node_to_scc_id;
          if next_node != node then collect_scc ()
        in
        collect_scc ();
        sccs := ISet.add scc_id !sccs
      )
    in

    Set.iter (fun node -> if not (is_visited node) then strong_connect node) nodes;
    (sccs, node_to_scc_id)

  (* Order all nodes in the graph that are reachable from the provided starting nodes. We attempt
     to generate an ordering that resembles the original graph and keeps adjacent nodes near each
     other.

     In general we would like to order via a depth-first topological sort when possible. This is
     complicated by the fact that the graph may contain cycles. To account for this we first compute
     the strongly connected components (SCCs) of the graph, which will form a DAG, and then visit
     them in depth-first topological order.

     Within each SCC we approximate a topological sort to the best of our ability. We keep track of
     the incoming degree for each node, and greedily visit nodes (in a depth-first traversal) if all
     their predecessors have been visited. Otherwise, if a node is in the current SCC but all its
     predecessors have not been visited we enqueue it in a frontier for the current SCC, and pop
     nodes off this queue to visit whenever we reach a dead end.

     If the entire current SCC has been visited (known because we reach a dead end and there are no
     nodes currently on the current SCC frontier), then we visit the next SCC in depth-first
     topological order.
  *)
  let order_nodes start_nodes =
    (* Depth-first traversal to gather all nodes *)
    let all_nodes = ref Set.empty in
    let rec visit_node node =
      if not (Set.mem node !all_nodes) then begin
        all_nodes := Set.add node !all_nodes;
        Node.iter_next node visit_node
      end
    in
    List.iter visit_node start_nodes;

    (* Run Tarjan's algorithm to find strongly connected components of control flow graph *)
    let (sccs, node_to_scc_id) = tarjans_scc !all_nodes in

    (* Build map of predecessor degree for each node, and map from SCC id the set of SCCs that
       precede it in the SCC DAG. *)
    let scc_preds = ref IMap.empty in
    let node_pred_degree = ref Map.empty in
    let add_scc_pred pred_scc_id (succ_node : Node.t) =
      let succ_scc_id = Map.find succ_node !node_to_scc_id in
      if pred_scc_id != succ_scc_id then
        scc_preds :=
          IMap.add succ_scc_id (ISet.add pred_scc_id (IMap.find succ_scc_id !scc_preds)) !scc_preds
    in
    let remove_scc_pred pred_scc_id (succ_node : Node.t) =
      let succ_scc_id = Map.find succ_node !node_to_scc_id in
      if pred_scc_id != succ_scc_id then
        scc_preds :=
          IMap.add
            succ_scc_id
            (ISet.remove pred_scc_id (IMap.find succ_scc_id !scc_preds))
            !scc_preds
    in
    let inc_node_pred (node : Node.t) =
      let old_degree = Map.find node !node_pred_degree in
      node_pred_degree := Map.add node (old_degree + 1) !node_pred_degree
    in
    let dec_node_pred (node : Node.t) =
      node_pred_degree := Map.add node (Map.find node !node_pred_degree - 1) !node_pred_degree
    in
    ISet.iter (fun scc_id -> scc_preds := IMap.add scc_id ISet.empty !scc_preds) !sccs;
    Set.iter (fun node -> node_pred_degree := Map.add node 0 !node_pred_degree) !all_nodes;
    Set.iter
      (fun node ->
        let scc_id = Map.find node !node_to_scc_id in
        Node.iter_next node (fun next_node ->
            inc_node_pred next_node;
            add_scc_pred scc_id next_node))
      !all_nodes;

    let nodes = ref [] in
    let visited_nodes = ref Set.empty in
    let is_visited node = Set.mem node !visited_nodes in

    (* Blocks in other SCCs that are ready to be visited (meaning all predecessor SCCs have already
       been visited). This is represented as a stack of queues, where each queue contains the nodes
       in other SCCs gathered during the traversal of a single SCC. A stack of queues is used to give
       an in-order traversal (the same traversal order as recursive DFS). *)
    let next_sccs_frontier = Stack.create () in
    (* Blocks within the current SCC that are ready to be visited (meaning all predecessor nodes have
       already been visited). *)
    let within_scc_frontier = Queue.create () in

    let rec iter ~prev_scc () =
      (* First try to visit a node within the current SCC *)
      match Queue.take_opt within_scc_frontier with
      | Some next_node ->
        if not (is_visited next_node) then visit_node ~scc_id:prev_scc next_node;
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
          | Some next_node ->
            (* If we are entering a new SCC, add a new queue to the stack to keep track of the next
               SCCs to visit from the new SCC. *)
            let next_scc = Map.find next_node !node_to_scc_id in
            if prev_scc != next_scc then Stack.push (Queue.create ()) next_sccs_frontier;
            if not (is_visited next_node) then visit_node ~scc_id:next_scc next_node;
            iter ~prev_scc:next_scc ()))
    and visit_node ~scc_id node =
      visited_nodes := Set.add node !visited_nodes;
      nodes := node :: !nodes;
      Node.iter_next node (fun next_node ->
          dec_node_pred next_node;
          remove_scc_pred scc_id next_node;
          visit_or_enqueue ~prev_scc_id:scc_id next_node)
    and visit_or_enqueue ~prev_scc_id next_node =
      let next_scc_id = Map.find next_node !node_to_scc_id in
      if is_visited next_node then
        ()
      else if prev_scc_id = next_scc_id then
        (* If the next node is within the same SCC, visit it immediately if all its predecessors
           have been visited, otherwise enqueue in within the current SCC's frontier. *)
        if Map.find next_node !node_pred_degree = 0 then
          visit_node ~scc_id:prev_scc_id next_node
        else
          Queue.push next_node within_scc_frontier
      else if ISet.cardinal (IMap.find next_scc_id !scc_preds) = 0 then
        (* If the next node is within another SCC whose predecessor SCCs have been visited then
           enqueue it in the frontier for new SCCs to visit *)
        Queue.push next_node (Stack.top next_sccs_frontier)
    in

    (* Start with a stack of a single queue containing the start node ids *)
    Stack.push (Queue.create ()) next_sccs_frontier;
    List.iter (fun start_node -> Queue.push start_node (Stack.top next_sccs_frontier)) start_nodes;

    List.iter
      (fun start_node -> iter ~prev_scc:(Map.find start_node !node_to_scc_id) ())
      start_nodes;
    List.rev !nodes
end

module OrderedCFG = Make (struct
  type t = Block.t

  module Set = BlockSet
  module Map = BlockMap
  let compare = Block.compare

  let iter_next block f =
    match get_terminator block with
    | Some { instr = Continue continue; _ } -> f continue
    | Some { instr = Branch { test = _; jump; continue }; _ } ->
      f continue;
      f jump
    | _ -> ()
end)

(* Return CFG of function starting at the start block *)
let get_ordered_cfg start_block = OrderedCFG.order_nodes [start_block]

module OrderedCallGraph = Make (struct
  type t = Function.t

  module Set = FunctionSet
  module Map = FunctionMap

  let compare = Function.compare

  let iter_next func f =
    func_iter_blocks func (fun block ->
        iter_instructions block (fun _ instr ->
            match instr.instr with
            | Call { func = Value use; _ } ->
              (match use.value.value with
              | Lit (Function next_func) -> f next_func
              | _ -> ())
            | _ -> ()))
end)

(* Return reverse ordering of call graph ending at main function *)
let get_ordered_call_graph func_roots = OrderedCallGraph.order_nodes func_roots |> List.rev
