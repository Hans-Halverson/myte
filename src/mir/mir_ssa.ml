open Basic_collections
open Mir
open Mir_builders
open Mir_type

(* Promotion of variables on stack into registers joined with Phi nodes

   During initial MIR emission variables are stored on the stack, where a Store to that stack
   location is considered a "write" and a Load from that stack location is considered a "use".
   For the purpose of this pass, variables are uniquely identified by the pointer of their location
   on the stack (the id of a StackAlloc instruction).

   Pass 1 (Identify join points):
     Propagate the last write instruction ids for each variable through the program. Identify the set
     of join points, where a join point is a (block, variable) pair meaning that there are multiple
     sources for the last write to the given variable which are joined at this block. This means the
     block is the earliest point where the variable was last written to in multiple control flow
     paths. Construct an empty phi chain node at each join point.

   Pass 2 (Build phi chain graph):
     Propagate the last write instruction ids for each variable through the program like before, but
     also count each saved variable join from the first pass as a "write". Use this to build a graph
     of phi chain nodes, with one phi chain node for each join point/(block, variable) pair from the
     previous pass. Phi chain nodes contain the set of last write sources as well as previous phi
     chain nodes. Phi chain nodes are initially "unrealized", meaning they may not be converted to
     a concrete phi node in the SSA IR.

     Also for each block, save the source for each variable currently in scope. The source will
     either be a concrete location or a phi chain node.

     Also for each variable use in each block save the source, which will be either be a concrete
     location or a phi chain node.

   Realize Phi Nodes:
     Determine the minimal set of phi nodes needed for the SSA IR. For each of the saved variables
     whose source is a phi, mark the phi node as realized (creating a phi instruction for it).

     Later, when determining the concrete write locations which are args to each phi node, we simply
     need to traverse the phi chain node graph and collecting all concrete write locations that are
     reachable, where realized phi chain nodes cannot be traversed through.

   Map to SSA IR:
     Map across IR, removing StackAlloc, Load, and Store instructions for variables that have been
     promoted to registers. Store instructions are converted to Movs of the stored value to the new
     SSA variable. Also must rewrite all use variables that were the result of removed Load
     instructions to now directly reference the new SSA variable. *)

module PhiChainNode = struct
  type id = int

  type t = {
    id: id;
    block: Block.t;
    (* Previous phi nodes in the phi chain graph, mapped to the previous block that they flowed from *)
    mutable prev_nodes: Block.t IMap.t;
    (* Store instruction ids for this node, mapped to the previous block that they flowed from along
       with the value that was stored. *)
    mutable write_locs: (Block.t * Value.t) IMap.t;
    mutable realized: Value.t option;
    (* Type is filled after creation *)
    mutable mir_type: Type.t option;
  }
end

type source =
  (* There was a single write at this store instruction in this block, writing this value *)
  | WriteLocation of Block.t * Instruction.t * Value.t
  (* Multiple writes were joined by phi chain node *)
  | PhiChainJoin of PhiChainNode.id

type cx = {
  mutable visited_blocks: BlockSet.t;
  mutable stack_alloc_ids: Type.t IMap.t;
  mutable phi_chain_nodes: PhiChainNode.t IMap.t;
  (* Blocks to the pointer ids with phi chain nodes for that block *)
  mutable block_nodes: PhiChainNode.id IMap.t BlockMap.t;
  (* Blocks to the set of phi chain node ids that are realized in that block *)
  mutable realized_phis: BlockIMMap.t;
  (* Function name to the (load instruction, local source) for each load instruction id *)
  mutable use_sources: (Value.t * source) IMap.t SMap.t;
}

let mk_cx () =
  {
    visited_blocks = BlockSet.empty;
    stack_alloc_ids = IMap.empty;
    phi_chain_nodes = IMap.empty;
    block_nodes = BlockMap.empty;
    realized_phis = BlockIMMap.empty;
    use_sources = SMap.empty;
  }

let max_phi_chain_node_id = ref 0

let mk_phi_chain_node_id () =
  let id = !max_phi_chain_node_id in
  max_phi_chain_node_id := id + 1;
  id

let mk_phi_chain_node ~cx block =
  let id = mk_phi_chain_node_id () in
  cx.phi_chain_nodes <-
    IMap.add
      id
      {
        PhiChainNode.id;
        block;
        prev_nodes = IMap.empty;
        write_locs = IMap.empty;
        realized = None;
        mir_type = None;
      }
      cx.phi_chain_nodes;
  id

let get_node ~cx node_id = IMap.find node_id cx.phi_chain_nodes

let rec promote_variables_to_registers ir =
  let cx = mk_cx () in
  find_join_points ~cx ir;
  build_phi_nodes ~cx ir;
  rewrite_program ~cx ir

and find_join_points ~cx program =
  let open Program in
  let block_sources = ref BlockMap.empty in
  (* Maintain set of all sources for each variable in each block *)
  let add_block_sources (block : Block.t) new_sources =
    let new_sources =
      IMap.fold
        (fun ptr_id store_instr_id sources ->
          IMap.add ptr_id (ISet.singleton store_instr_id) sources)
        new_sources
        IMap.empty
    in
    let sources =
      match BlockMap.find_opt block !block_sources with
      | None -> new_sources
      | Some sources ->
        IMap.union
          (fun _ sources1 sources2 -> Some (ISet.union sources1 sources2))
          sources
          new_sources
    in
    block_sources := BlockMap.add block sources !block_sources
  in
  let rec visit_block ~sources block =
    let maybe_visit_block sources (next_block : Block.t) =
      if BlockSet.mem next_block cx.visited_blocks then
        add_block_sources next_block sources
      else
        visit_block ~sources next_block
    in
    cx.visited_blocks <- BlockSet.add block cx.visited_blocks;
    add_block_sources block sources;
    (* Collect all declaration sources in block *)
    let sources = ref sources in
    iter_instructions block (fun _ instr ->
        match instr.instr with
        | StackAlloc ty -> cx.stack_alloc_ids <- IMap.add instr.id ty cx.stack_alloc_ids
        | Store ({ value = { value = Instr { id; _ } | Argument { id; _ }; _ }; _ }, _)
          when IMap.mem id cx.stack_alloc_ids ->
          sources := IMap.add id instr.id !sources
        | _ -> ());
    let next_blocks = get_next_blocks block in
    BlockSet.iter (fun next_block -> maybe_visit_block !sources next_block) next_blocks
  in
  (* Visit all function bodies *)
  SMap.iter
    (fun _ { Function.start_block; _ } -> visit_block ~sources:IMap.empty start_block)
    program.funcs;
  (* Create phi chain nodes for all join points in program *)
  cx.block_nodes <-
    BlockMap.fold
      (fun block sources block_nodes ->
        let nodes =
          IMap.fold
            (fun ptr_id sources nodes ->
              if ISet.cardinal sources <= 1 then
                nodes
              else
                let node_id = mk_phi_chain_node ~cx block in
                IMap.add ptr_id node_id nodes)
            sources
            IMap.empty
        in
        if IMap.is_empty nodes then
          block_nodes
        else
          BlockMap.add block nodes block_nodes)
      !block_sources
      BlockMap.empty

and add_use_source ~cx func_name load_instr_id source =
  let use_sources = SMap.find func_name cx.use_sources in
  let new_use_sources = IMap.add load_instr_id source use_sources in
  cx.use_sources <- SMap.add func_name new_use_sources cx.use_sources

and build_phi_nodes ~cx program =
  let open Program in
  (* Update phi nodes at the given block with the new sources. If source is a write locations add
     it directly to the phi node. Otherwise if the source is another phi node then add link to
     create phi chain. *)
  let update_phis_from_sources (block : Block.t) (prev_block : Block.t) new_sources =
    match BlockMap.find_opt block cx.block_nodes with
    | None -> ()
    | Some nodes ->
      IMap.iter
        (fun ptr_id source ->
          match IMap.find_opt ptr_id nodes with
          | None -> ()
          | Some node_id ->
            let node = get_node ~cx node_id in
            (* Propagate mir types from write locations through phi chain nodes *)
            (match source with
            | WriteLocation (_, store_instr, arg_val) ->
              if not (IMap.mem store_instr.id node.write_locs) then (
                node.write_locs <- IMap.add store_instr.id (prev_block, arg_val) node.write_locs;
                node.mir_type <- Some (type_of_value arg_val)
              )
            | PhiChainJoin prev_node ->
              if prev_node <> node_id && not (IMap.mem prev_node node.prev_nodes) then (
                node.prev_nodes <- IMap.add prev_node prev_block node.prev_nodes;
                match (get_node ~cx prev_node).mir_type with
                | None -> ()
                | Some _ as mir_type -> node.mir_type <- mir_type
              )))
        new_sources
  in
  let phi_nodes_to_realize = ref IMap.empty in
  let rec visit_block ~func_name ~sources ~prev_block (block : Block.t) =
    let maybe_visit_block sources (next_block : Block.t) =
      if BlockSet.mem next_block cx.visited_blocks then
        update_phis_from_sources next_block block sources
      else
        visit_block ~func_name ~sources ~prev_block:block next_block
    in
    cx.visited_blocks <- BlockSet.add block cx.visited_blocks;
    update_phis_from_sources block prev_block sources;
    let sources = ref sources in
    (* Add phi chain join nodes as current sources *)
    begin
      match BlockMap.find_opt block cx.block_nodes with
      | None -> ()
      | Some nodes ->
        IMap.iter
          (fun ptr_id node_id -> sources := IMap.add ptr_id (PhiChainJoin node_id) !sources)
          nodes
    end;
    (* Mark phi nodes to realize *)
    iter_instructions block (fun instr_val instr ->
        match instr.instr with
        | Store ({ value = { value = Instr { id; _ } | Argument { id; _ }; _ }; _ }, arg)
          when IMap.mem id cx.stack_alloc_ids ->
          (* Source for this variable is now this write location *)
          sources := IMap.add id (WriteLocation (instr.block, instr, arg.value)) !sources
        | Load { value = { value = Instr { id; _ } | Argument { id; _ }; _ }; _ }
          when IMap.mem id cx.stack_alloc_ids ->
          (* Save source for each use *)
          let source = IMap.find id !sources in
          add_use_source ~cx func_name instr.id (instr_val, source);
          (* If source is a phi node it should be realized *)
          (match source with
          | WriteLocation _ -> ()
          | PhiChainJoin node_id ->
            phi_nodes_to_realize := IMap.add node_id id !phi_nodes_to_realize)
        | _ -> ());
    let next_blocks = get_next_blocks block in
    BlockSet.iter (fun next_block -> maybe_visit_block !sources next_block) next_blocks
  in
  (* Visit bodies of all functions *)
  cx.visited_blocks <- BlockSet.empty;
  SMap.iter
    (fun _ { Function.name = func_name; start_block; _ } ->
      cx.use_sources <- SMap.add func_name IMap.empty cx.use_sources;
      visit_block ~func_name ~sources:IMap.empty ~prev_block:start_block start_block)
    program.funcs;
  (* To realize a phi node, that phi node and its entire phi chain graph should be realized *)
  let rec realize_phi_chain_graph node_id ptr_id =
    let node = get_node ~cx node_id in
    if Option.is_none node.realized then (
      (* Realize by creating phi instruction *)
      let type_ = IMap.find ptr_id cx.stack_alloc_ids in
      let phi_instr = mk_blockless_phi ~type_ ~args:BlockMap.empty in
      node.realized <- Some phi_instr;
      cx.realized_phis <- BlockIMMap.add node.block node_id cx.realized_phis;
      IMap.iter (fun prev_node_id _ -> realize_phi_chain_graph prev_node_id ptr_id) node.prev_nodes
    )
  in
  IMap.iter realize_phi_chain_graph !phi_nodes_to_realize

and rewrite_program ~cx program =
  (* Traverse phi chain graph to gather all previous block/value pairs for a given phi node. The phi
     chain graph is deeply traversed until realized nodes are encountered. *)
  let gather_phi_args ~(phi_val : Value.t) ~(phi : Instruction.Phi.t) (node_id : PhiChainNode.id) =
    let add_phi_arg prev_block arg_val =
      phi_add_arg ~phi_val ~phi ~block:prev_block ~value:arg_val
    in
    let rec visit_phi_node node_id prev_block =
      let node = get_node ~cx node_id in
      match node.realized with
      (* Do not descend into realized nodes, use their realized instruction *)
      | Some instr -> add_phi_arg prev_block instr
      (* Descend into unrealized nodes, gather local writes and phi chain nodes *)
      | _ ->
        IMap.iter (fun _ (_, arg_val) -> add_phi_arg prev_block arg_val) node.write_locs;
        IMap.iter (fun node_id _ -> visit_phi_node node_id prev_block) node.prev_nodes
    in
    let node = get_node ~cx node_id in
    IMap.iter (fun _ (prev_block, arg_val) -> add_phi_arg prev_block arg_val) node.write_locs;
    IMap.iter visit_phi_node node.prev_nodes
  in

  let visit_block (block : Block.t) =
    (* Create and add phis to beginning of block *)
    let phis =
      let realized_phis = BlockIMMap.find_all block cx.realized_phis in
      BlockIMMap.VSet.fold
        (fun node_id phis ->
          let node = get_node ~cx node_id in
          let phi_instr = Option.get node.realized in
          let phi = cast_to_phi (cast_to_instruction phi_instr) in
          gather_phi_args ~phi_val:phi_instr ~phi node_id;
          phi_instr :: phis)
        realized_phis
        []
    in
    List.iter (fun instr -> prepend_instruction block instr) phis;
    (* Remove memory instructions for memory locations promoted to registers *)
    filter_instructions block (fun instr ->
        match instr.Instruction.instr with
        | StackAlloc _ when IMap.mem instr.id cx.stack_alloc_ids -> false
        | Store ({ value = { value = Instr { id; _ } | Argument { id; _ }; _ }; _ }, _)
        | Load { value = { value = Instr { id; _ } | Argument { id; _ }; _ }; _ }
          when IMap.mem id cx.stack_alloc_ids ->
          false
        | _ -> true)
  in

  SMap.iter
    (fun _ func ->
      (* Create phi nodes within function *)
      func_iter_blocks func visit_block;

      (* Find all rewrites between values, rewriting uses of a load from a promoted memory location
         to the value that was last stored before the load. *)
      let use_sources = SMap.find func.name cx.use_sources in
      let rewrite_map =
        IMap.fold
          (fun _ (load_instr, source) value_map ->
            match source with
            | WriteLocation (_, _, write_val) -> VMap.add load_instr write_val value_map
            | PhiChainJoin node_id ->
              let node = get_node ~cx node_id in
              let mapped_val = Option.get node.realized in
              VMap.add load_instr mapped_val value_map)
          use_sources
          VMap.empty
      in

      (* Follow successive rewrites to create final rewrite map *)
      let rec find_final_value value =
        match VMap.find_opt value rewrite_map with
        | None -> value
        | Some mapped_value -> find_final_value mapped_value
      in
      let final_rewrite_map = VMap.map (fun to_val -> find_final_value to_val) rewrite_map in

      (* Instructions that have been replaced by another value so that we can update mapped values
         in the final_rewrite_map otherwise we could try to value_replace_uses with a to_ that should
         have been replaced with another value. *)
      let replaced_instrs_map = ref VMap.empty in
      set_replace_instruction_hook
        (Some (fun from to_ -> replaced_instrs_map := VMap.add from to_ !replaced_instrs_map));

      (* Rewrite uses of values in program *)
      VMap.iter
        (fun load_instr write_val ->
          let to_ =
            match VMap.find_opt write_val !replaced_instrs_map with
            | Some replaced_write_val -> replaced_write_val
            | None -> write_val
          in
          value_replace_uses ~from:load_instr ~to_)
        final_rewrite_map)
    program.funcs;

  set_replace_instruction_hook None;

  (* Strip empty blocks *)
  program_iter_blocks program block_remove_if_empty;

  program
