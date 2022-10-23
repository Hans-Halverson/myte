module Make (IdType : MultiMap.KEY_AND_VALUE_TYPE) = struct
  module Map = IdType.Map
  module Set = IdType.Set

  module MMap = MultiMap.Make (Map) (Set)

  exception CycleException of IdType.t

  type t = {
    mutable nodes: Set.t;
    mutable edges: MMap.t;
  }

  let mk () = { nodes = Set.empty; edges = MMap.empty }

  let add_node ~graph node = graph.nodes <- Set.add node graph.nodes

  let add_edge ~graph src dest = graph.edges <- MMap.add src dest graph.edges

  (* Return the a graph with nodes in a valid topological order. If the graph contains cycles,
     raise a CycleException. *)
  let topological_sort ~graph =
    let unvisited_nodes = ref graph.nodes in
    let in_progress_nodes = ref Set.empty in
    let finished_nodes = ref Set.empty in
    let sorted_nodes = ref [] in

    let move_sets node old_set new_set =
      old_set := Set.remove node !old_set;
      new_set := Set.add node !new_set
    in

    (* Visit all nodes in depth first order, sorting topologically and erroring on cycles *)
    let rec visit node =
      if Set.mem node !finished_nodes then
        ()
      else if Set.mem node !in_progress_nodes then
        raise (CycleException node)
      else (
        move_sets node unvisited_nodes in_progress_nodes;
        let next_nodes = MMap.find_all node graph.edges in
        Set.iter visit next_nodes;
        move_sets node in_progress_nodes finished_nodes;
        sorted_nodes := node :: !sorted_nodes
      )
    in

    while not (Set.is_empty !unvisited_nodes) do
      visit (Set.choose !unvisited_nodes)
    done;

    !sorted_nodes
end

module LocGraph = Make (Loc)
