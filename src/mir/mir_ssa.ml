open Basic_collections
open Mir
open Mir_mapper
open Mir_type
open Mir_visitor

(* Promotion of variables on stack into registers joined with Phi nodes

  During initial MIR emission variables are stored on the stack, where a Store to that stack
  location is considered a "write" and a Load from that stack location is considered a "use".
  For the purpose of this pass, variables are uniquely identified by the pointer of their location
  on the stack.

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
    whose source is a phi, mark the phi node as realized (giving it a variable id).
    
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
    block_id: Block.id;
    (* Previous phi nodes in the phi chain graph, mapped to the previous block that they flowed from *)
    mutable prev_nodes: Block.id IMap.t;
    (* Store instruction ids for this node, mapped to the previous block that they flowed from along
       with the value that was stored. *)
    mutable write_locs: (Block.id * Value.t) IMap.t;
    mutable realized: Instruction.t option;
    (* Type is filled after creation *)
    mutable mir_type: Type.t option;
  }
end

type source =
  (* There was a single write at this store instruction in this block, writing this value *)
  | WriteLocation of Block.id * Value.id * Value.t
  (* Multiple writes were joined by phi chain node *)
  | PhiChainJoin of PhiChainNode.id

type cx = {
  mutable blocks: Block.t IMap.t;
  mutable visited_blocks: ISet.t;
  mutable stack_alloc_ids: Type.t IMap.t;
  mutable phi_chain_nodes: PhiChainNode.t IMap.t;
  (* Block ids to the pointer var ids with phi chain nodes for that block *)
  mutable block_nodes: PhiChainNode.id IMap.t IMap.t;
  (* Block ids to the set of phi chain node ids that are realized in that block *)
  mutable realized_phis: IIMMap.t;
  (* Function name to the local source for each load var id *)
  mutable use_sources: source IMap.t SMap.t;
  (* Block ids to the set of blocks that precede that block *)
  mutable prev_blocks: ISet.t IMap.t;
}

let mk_cx () =
  {
    blocks = IMap.empty;
    visited_blocks = ISet.empty;
    stack_alloc_ids = IMap.empty;
    phi_chain_nodes = IMap.empty;
    block_nodes = IMap.empty;
    realized_phis = IIMMap.empty;
    use_sources = SMap.empty;
    prev_blocks = IMap.empty;
  }

let max_phi_chain_node_id = ref 0

let mk_phi_chain_node_id () =
  let id = !max_phi_chain_node_id in
  max_phi_chain_node_id := id + 1;
  id

let mk_phi_chain_node ~cx block_id =
  let id = mk_phi_chain_node_id () in
  cx.phi_chain_nodes <-
    IMap.add
      id
      {
        PhiChainNode.id;
        block_id;
        prev_nodes = IMap.empty;
        write_locs = IMap.empty;
        realized = None;
        mir_type = None;
      }
      cx.phi_chain_nodes;
  id

let get_node ~cx node_id = IMap.find node_id cx.phi_chain_nodes

let add_block_link ~cx prev_id next_id = cx.prev_blocks <- IIMMap.add next_id prev_id cx.prev_blocks

let rec promote_variables_to_registers ir =
  let cx = mk_cx () in
  find_join_points ~cx ir;
  build_phi_nodes ~cx ir;
  rewrite_program ~cx ir

(* A visitor used in find_join_points, which collects all declaration sources *)
and mk_add_sources_visitor ~cx ~program sources =
  object
    inherit IRVisitor.t ~program

    method! visit_instruction ~block:_ instr =
      match instr.instr with
      | StackAlloc ty -> cx.stack_alloc_ids <- IMap.add instr.id ty cx.stack_alloc_ids
      | Store ((Instr { id; _ } | Argument { id; _ }), _) when IMap.mem id cx.stack_alloc_ids ->
        sources := IMap.add id instr.id !sources
      | _ -> ()
  end

and find_join_points ~cx program =
  let open Program in
  let block_sources = ref IMap.empty in
  (* Maintain set of all sources for each variable in each block *)
  let add_block_sources block_id new_sources =
    let new_sources =
      IMap.fold
        (fun var_id store_instr_id sources ->
          IMap.add var_id (ISet.singleton store_instr_id) sources)
        new_sources
        IMap.empty
    in
    let sources =
      match IMap.find_opt block_id !block_sources with
      | None -> new_sources
      | Some sources ->
        IMap.union
          (fun _ sources1 sources2 -> Some (ISet.union sources1 sources2))
          sources
          new_sources
    in
    block_sources := IMap.add block_id sources !block_sources
  in
  let rec visit_block ~sources block_id =
    let maybe_visit_block sources next_block_id =
      if ISet.mem next_block_id cx.visited_blocks then
        add_block_sources next_block_id sources
      else
        visit_block ~sources next_block_id
    in
    cx.visited_blocks <- ISet.add block_id cx.visited_blocks;
    add_block_sources block_id sources;
    let sources = ref sources in
    let visitor = mk_add_sources_visitor ~cx ~program sources in
    let block = IMap.find block_id program.blocks in
    List.iter (visitor#visit_instruction ~block) block.instructions;
    match block.next with
    | Halt -> ()
    | Continue continue ->
      add_block_link ~cx block_id continue;
      maybe_visit_block !sources continue
    | Branch { continue; jump; _ } ->
      add_block_link ~cx block_id continue;
      add_block_link ~cx block_id jump;
      maybe_visit_block !sources continue;
      maybe_visit_block !sources jump
  in
  (* Visit all function bodies *)
  SMap.iter
    (fun _ { Function.body_start_block; _ } -> visit_block ~sources:IMap.empty body_start_block)
    program.funcs;
  (* Create phi chain nodes for all join points in program *)
  cx.block_nodes <-
    IMap.fold
      (fun block_id sources block_nodes ->
        let nodes =
          IMap.fold
            (fun var_id sources nodes ->
              if ISet.cardinal sources <= 1 then
                nodes
              else
                let node_id = mk_phi_chain_node ~cx block_id in
                IMap.add var_id node_id nodes)
            sources
            IMap.empty
        in
        if IMap.is_empty nodes then
          block_nodes
        else
          IMap.add block_id nodes block_nodes)
      !block_sources
      IMap.empty

and add_use_source ~cx func_name load_instr_id source =
  let use_sources = SMap.find func_name cx.use_sources in
  let new_use_sources = IMap.add load_instr_id source use_sources in
  cx.use_sources <- SMap.add func_name new_use_sources cx.use_sources

and mk_build_phi_nodes_visitor ~cx program func_name sources phi_nodes_to_realize =
  object
    inherit IRVisitor.t ~program

    method! visit_instruction ~block instr =
      match instr.instr with
      | Store ((Instr { id; _ } | Argument { id; _ }), arg) when IMap.mem id cx.stack_alloc_ids ->
        (* Source for this variable is now this write location *)
        sources := IMap.add id (WriteLocation (block.id, instr.id, arg)) !sources
      | Load (Instr { id; _ } | Argument { id; _ }) when IMap.mem id cx.stack_alloc_ids ->
        (* Save source for each use *)
        let source = IMap.find id !sources in
        add_use_source ~cx func_name instr.id source;
        (* If source is a phi node it should be realized *)
        (match source with
        | WriteLocation _ -> ()
        | PhiChainJoin node_id -> phi_nodes_to_realize := IMap.add node_id id !phi_nodes_to_realize)
      | _ -> ()
  end

and build_phi_nodes ~cx program =
  let open Program in
  (* Update phi nodes at the given block with the new sources. If source is a write locations add
     it directly to the phi node. Otherwise if the source is another phi node then add link to
     create phi chain. *)
  let update_phis_from_sources block_id prev_block_id new_sources =
    match IMap.find_opt block_id cx.block_nodes with
    | None -> ()
    | Some decl_nodes ->
      IMap.iter
        (fun var_id source ->
          match IMap.find_opt var_id decl_nodes with
          | None -> ()
          | Some node_id ->
            let node = get_node ~cx node_id in
            (* Propagate mir types from write locations through phi chain nodes *)
            (match source with
            | WriteLocation (_, store_instr_id, arg_val) ->
              if not (IMap.mem store_instr_id node.write_locs) then (
                node.write_locs <- IMap.add store_instr_id (prev_block_id, arg_val) node.write_locs;
                node.mir_type <- Some (type_of_value arg_val)
              )
            | PhiChainJoin prev_node ->
              if prev_node <> node_id && not (IMap.mem prev_node node.prev_nodes) then (
                node.prev_nodes <- IMap.add prev_node prev_block_id node.prev_nodes;
                match (get_node ~cx prev_node).mir_type with
                | None -> ()
                | Some _ as mir_type -> node.mir_type <- mir_type
              )))
        new_sources
  in
  let phi_nodes_to_realize = ref IMap.empty in
  let rec visit_block ~func_name ~sources ~prev_block_id block_id =
    let maybe_visit_block sources next_block_id =
      if ISet.mem next_block_id cx.visited_blocks then
        update_phis_from_sources next_block_id block_id sources
      else
        visit_block ~func_name ~sources ~prev_block_id:block_id next_block_id
    in
    cx.visited_blocks <- ISet.add block_id cx.visited_blocks;
    update_phis_from_sources block_id prev_block_id sources;
    let sources = ref sources in
    (* Add phi chain join nodes as current sources *)
    begin
      match IMap.find_opt block_id cx.block_nodes with
      | None -> ()
      | Some decl_nodes ->
        IMap.iter
          (fun var_id node_id -> sources := IMap.add var_id (PhiChainJoin node_id) !sources)
          decl_nodes
    end;
    let block = IMap.find block_id program.blocks in
    let visitor = mk_build_phi_nodes_visitor ~cx program func_name sources phi_nodes_to_realize in
    List.iter (visitor#visit_instruction ~block) block.instructions;
    match block.next with
    | Halt -> ()
    | Continue continue -> maybe_visit_block !sources continue
    | Branch { test; continue; jump } ->
      visitor#visit_value ~block test;
      maybe_visit_block !sources continue;
      maybe_visit_block !sources jump
  in
  (* Visit bodies of all functions *)
  cx.visited_blocks <- ISet.empty;
  SMap.iter
    (fun _ { Function.name = func_name; body_start_block; _ } ->
      cx.use_sources <- SMap.add func_name IMap.empty cx.use_sources;
      visit_block ~func_name ~sources:IMap.empty ~prev_block_id:body_start_block body_start_block)
    program.funcs;
  (* To realize a phi node, that phi node and its entire phi chain graph should be realized *)
  let rec realize_phi_chain_graph node_id decl_loc =
    let node = get_node ~cx node_id in
    if Option.is_none node.realized then (
      (* Realize by creating phi instruction *)
      let type_ = IMap.find decl_loc cx.stack_alloc_ids in
      let phi = { Instruction.Phi.args = IMap.empty } in
      let phi_instr = { Instruction.id = mk_value_id (); type_; instr = Phi phi } in
      node.realized <- Some phi_instr;
      cx.realized_phis <- IIMMap.add node.block_id node_id cx.realized_phis;
      IMap.iter
        (fun prev_node_id _ -> realize_phi_chain_graph prev_node_id decl_loc)
        node.prev_nodes
    )
  in
  IMap.iter realize_phi_chain_graph !phi_nodes_to_realize

and rewrite_program ~cx program =
  cx.visited_blocks <- ISet.empty;
  let rec visit_block ~name block_id =
    cx.visited_blocks <- ISet.add block_id cx.visited_blocks;
    let maybe_visit_block next_block_id =
      if ISet.mem next_block_id cx.visited_blocks then
        ()
      else
        visit_block ~name next_block_id
    in
    let block = IMap.find block_id program.blocks in
    (* Create and add phis to beginning of block *)
    let phis =
      match IMap.find_opt block_id cx.realized_phis with
      | None -> []
      | Some realized_phis ->
        let phis =
          ISet.fold
            (fun node_id phis ->
              let node = get_node ~cx node_id in
              let phi_instr = Option.get node.realized in
              let phi = cast_to_phi phi_instr in
              phi.args <- gather_phi_args node_id;
              phi_instr :: phis)
            realized_phis
            []
        in
        List.rev phis
    in
    block.instructions <- phis @ block.instructions;
    (* Remove memory instructions for memory locations promoted to registers *)
    block.instructions <-
      List.filter
        (fun instr ->
          match instr.Instruction.instr with
          | StackAlloc _ when IMap.mem instr.id cx.stack_alloc_ids -> false
          | Store ((Instr { id; _ } | Argument { id; _ }), _)
          | Load (Instr { id; _ } | Argument { id; _ })
            when IMap.mem id cx.stack_alloc_ids ->
            false
          | _ -> true)
        block.instructions;
    block.next <-
      (let open Block in
      match block.next with
      | Halt -> Halt
      | Continue continue ->
        maybe_visit_block continue;
        Continue continue
      | Branch { test; continue; jump } ->
        maybe_visit_block continue;
        maybe_visit_block jump;
        Branch { test; continue; jump });
    cx.blocks <- IMap.add block.id block cx.blocks
  (* Traverse phi chain graph to gather all variable ids for a given phi node. The phi chain
     graph is deeply traversed until realized nodes are encountered. *)
  and gather_phi_args (node_id : PhiChainNode.id) : Value.t IMap.t =
    let phi_args = ref IMap.empty in
    let add_phi_arg prev_block_id arg_val = phi_args := IMap.add prev_block_id arg_val !phi_args in
    let rec visit_phi_node node_id prev_block_id =
      let node = get_node ~cx node_id in
      match node.realized with
      (* Do not descend into realized nodes, use their realized var id *)
      | Some instr -> add_phi_arg prev_block_id (Value.Instr instr)
      (* Descend into unrealized nodes, gather local writes and phi chain nodes *)
      | _ ->
        IMap.iter (fun _ (_, arg_val) -> add_phi_arg prev_block_id arg_val) node.write_locs;
        IMap.iter (fun node_id _ -> visit_phi_node node_id prev_block_id) node.prev_nodes
    in
    let node = get_node ~cx node_id in
    IMap.iter (fun _ (prev_block_id, arg_val) -> add_phi_arg prev_block_id arg_val) node.write_locs;
    IMap.iter visit_phi_node node.prev_nodes;
    !phi_args
  in

  cx.visited_blocks <- ISet.empty;
  SMap.iter
    (fun _ ({ Function.name; body_start_block; _ } as func) ->
      (* Create phi nodes within function *)
      visit_block ~name body_start_block;

      (* Rewrite uses of a load from a promoted memory location to the last store before the load *)
      let use_sources = SMap.find name cx.use_sources in
      let var_map =
        IMap.fold
          (fun load_instr_id source var_map ->
            match source with
            | WriteLocation (_, _, write_val) -> IMap.add load_instr_id write_val var_map
            | PhiChainJoin node_id ->
              let node = get_node ~cx node_id in
              let instr = Option.get node.realized in
              let mapped_val = Value.Instr instr in
              IMap.add load_instr_id mapped_val var_map)
          use_sources
          IMap.empty
      in
      let rewrite_val_mapper = new rewrite_vals_mapper ~program var_map in
      rewrite_val_mapper#map_function func)
    program.funcs;

  (* Strip empty blocks *)
  IMap.iter
    (fun block_id { Block.instructions; next; func; _ } ->
      match next with
      (* A block can only be removed if it has no instructions, and if its next block has no phis
         (as the phis may reference this block). *)
      | Continue continue_id
        when instructions = [] && not (block_has_phis (IMap.find continue_id cx.blocks)) ->
        let prev_blocks =
          match IMap.find_opt block_id cx.prev_blocks with
          | None -> ISet.empty
          | Some prev_blocks -> prev_blocks
        in
        (* Block may be first block of function, reset body start block if so *)
        let func = SMap.find func program.funcs in
        if func.body_start_block = block_id then func.body_start_block <- continue_id;
        (* Previous nodes point to next node *)
        ISet.iter
          (fun prev_block_id ->
            let map_next_id id =
              if id = block_id then
                continue_id
              else
                id
            in
            let block = IMap.find prev_block_id cx.blocks in
            let next' =
              match block.next with
              | Halt -> Block.Halt
              | Continue continue -> Continue (map_next_id continue)
              | Branch { test; continue; jump } ->
                let new_continue = map_next_id continue in
                let new_jump = map_next_id jump in
                (* If both branches points to same label convert to continue *)
                if new_continue = new_jump then
                  Continue new_continue
                else
                  (* Otherwise create branch to new block *)
                  Branch { test; continue = new_continue; jump = new_jump }
            in
            block.next <- next')
          prev_blocks;
        (* Next node may have phis that point the removed block. Rewrite them to instead point to
           previous blocks. *)
        let next_node = IMap.find continue_id cx.blocks in
        block_iter_phis next_node (fun ({ args; _ } as phi) ->
            match IMap.find_opt block_id args with
            | None -> ()
            | Some arg_var_id ->
              let args' = IMap.remove block_id args in
              phi.args <-
                ISet.fold (fun prev_block_id -> IMap.add prev_block_id arg_var_id) prev_blocks args');
        (* Remove this empty node *)
        cx.blocks <- IMap.remove block_id cx.blocks;
        cx.prev_blocks <- IMap.remove block_id cx.prev_blocks;
        (* Remove the empty node from continue block's prev blocks *)
        let continue_prev_blocks = IMap.find continue_id cx.prev_blocks in
        let new_continue_prev_blocks =
          ISet.union prev_blocks (ISet.remove block_id continue_prev_blocks)
        in
        cx.prev_blocks <- IMap.add continue_id new_continue_prev_blocks cx.prev_blocks
      | _ -> ())
    cx.blocks;

  { program with blocks = cx.blocks }
