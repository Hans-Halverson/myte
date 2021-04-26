open Basic_collections
open Mir

(* Conversion of Control Flow IR to SSA IR

  Pass 1 (Identify join points):
    Propagate the last write locs for each variable through the program. Identify the set of join
    points, where a join point is a (block, variable) pair meaning that there are multiple sources
    for the last write to the given variable which are joined at this block. This means the block
    is the earliest point where the variable was last written to in multiple control flow paths.
    Construct an empty phi chain node at each join point.
 
  Pass 2 (Build phi chain graph):
    Propagate the last write locs for each variable through the program like before, but also count
    each saved variable join from the first pass as a "write". Use this to build a graph of phi
    chain nodes, with one phi chain node for each join point/(block, variable) pair from the
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
    Map across control flow IR, filling in control flow variables with concrete variable ids.
    Writes are uniquely numbered. For reads from a write within the same block simply refer to
    that write. For reads from a previous block look up the source in the saved map, which may
    be either a concrete write location or a realized phi node. *)

module PhiChainNode = struct
  type id = int

  type t = {
    id: id;
    block_id: Block.id;
    (* Previous phi nodes in the phi chain graph, mapped to the previous block that they flowed from *)
    mutable prev_nodes: Block.id IMap.t;
    (* Write locations for this node, mapped to the previous block that they flowed from *)
    mutable write_locs: Block.id LocMap.t;
    mutable realized: var_id option;
  }
end

type source =
  (* There was a single write at this loc in this block *)
  | WriteLocation of Block.id * Loc.t
  (* Multiple writes were joined by phi chain node *)
  | PhiChainJoin of PhiChainNode.id

type cx = {
  mutable blocks: ssa_block IMap.t;
  mutable visited_blocks: ISet.t;
  mutable phi_chain_nodes: PhiChainNode.t IMap.t;
  (* Block ids to the decl locs with phi chain nodes for that block *)
  mutable block_nodes: PhiChainNode.id LocMap.t IMap.t;
  (* Block ids to the decl locs to the var id for this block's phi nodes *)
  mutable realized_phis: PhiChainNode.id LocMap.t IMap.t;
  (* Local source for each use loc *)
  mutable use_sources: source LocMap.t;
  (* Write loc to (potentially new) var id *)
  mutable write_var_ids: var_id LocMap.t;
  (* Block ids to the set of blocks that precede that block *)
  mutable prev_blocks: ISet.t IMap.t;
}

let mk_cx () =
  {
    blocks = IMap.empty;
    visited_blocks = ISet.empty;
    phi_chain_nodes = IMap.empty;
    block_nodes = IMap.empty;
    realized_phis = IMap.empty;
    use_sources = LocMap.empty;
    write_var_ids = LocMap.empty;
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
        write_locs = LocMap.empty;
        realized = None;
      }
      cx.phi_chain_nodes;
  id

let get_node ~cx node_id = IMap.find node_id cx.phi_chain_nodes

let add_realized_phi ~cx block_id decl_loc node_id =
  let phis =
    match IMap.find_opt block_id cx.realized_phis with
    | None -> LocMap.singleton decl_loc node_id
    | Some decls -> LocMap.add decl_loc node_id decls
  in
  cx.realized_phis <- IMap.add block_id phis cx.realized_phis

let add_block_link ~cx prev_id next_id = cx.prev_blocks <- IIMMap.add next_id prev_id cx.prev_blocks

let rec control_flow_ir_to_ssa pcx ir =
  let cx = mk_cx () in
  find_join_points ~pcx ~cx ir;
  build_phi_nodes ~pcx ~cx ir;
  map_to_ssa ~pcx ~cx ir

and find_join_points ~pcx ~cx program =
  let open Program in
  let block_sources = ref IMap.empty in
  (* Maintain set of all sources for each variable in each block *)
  let add_block_sources block_id new_sources =
    let new_sources =
      LocMap.fold
        (fun decl_loc use_loc sources -> LocMap.add decl_loc (LocSet.singleton use_loc) sources)
        new_sources
        LocMap.empty
    in
    let sources =
      match IMap.find_opt block_id !block_sources with
      | None -> new_sources
      | Some sources ->
        LocMap.union
          (fun _ sources1 sources2 -> Some (LocSet.union sources1 sources2))
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
    let block = IMap.find block_id program.blocks in
    List.iter (visit_instruction ~sources) block.instructions;
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
  and visit_instruction ~sources (_, instruction) =
    let open Instruction in
    match instruction with
    | Ret _
    | StoreGlobal _ ->
      ()
    | Mov (result, _)
    | Call (result, _, _, _)
    | LoadGlobal (result, _)
    | Neg (result, _)
    | LogNot (result, _)
    | LogAnd (result, _, _)
    | LogOr (result, _, _)
    | Add (result, _, _)
    | Sub (result, _, _)
    | Mul (result, _, _)
    | Div (result, _, _)
    | Eq (result, _, _)
    | Neq (result, _, _)
    | Lt (result, _, _)
    | LtEq (result, _, _)
    | Gt (result, _, _)
    | GtEq (result, _, _) ->
      (match result with
      | Id _ -> ()
      | Local write_loc ->
        let decl_loc =
          Bindings.get_source_decl_loc_from_value_use pcx.Lex_analyze.bindings write_loc
        in
        sources := LocMap.add decl_loc write_loc !sources)
  in
  (* Visit all function bodies *)
  SMap.iter
    (fun _ { Function.params; body_start_block; _ } ->
      let sources =
        List.fold_left (fun sources (loc, _, _) -> LocMap.add loc loc sources) LocMap.empty params
      in
      visit_block ~sources body_start_block)
    program.funcs;
  (* Create phi chain nodes for all join points in program *)
  cx.block_nodes <-
    IMap.fold
      (fun block_id sources block_nodes ->
        let nodes =
          LocMap.fold
            (fun decl_loc sources nodes ->
              if LocSet.cardinal sources <= 1 then
                nodes
              else
                let node_id = mk_phi_chain_node ~cx block_id in
                LocMap.add decl_loc node_id nodes)
            sources
            LocMap.empty
        in
        if LocMap.is_empty nodes then
          block_nodes
        else
          IMap.add block_id nodes block_nodes)
      !block_sources
      IMap.empty

and build_phi_nodes ~pcx ~cx program =
  let open Program in
  (* Update phi nodes at the given block with the new sources. If source is a write locations add
     it directly to the phi node. Otherwise if the source is another phi node then add link to
     create phi chain. *)
  let update_phis_from_sources block_id prev_block_id new_sources =
    match IMap.find_opt block_id cx.block_nodes with
    | None -> ()
    | Some decl_nodes ->
      LocMap.iter
        (fun decl_loc source ->
          match LocMap.find_opt decl_loc decl_nodes with
          | None -> ()
          | Some node_id ->
            let node = get_node ~cx node_id in
            (match source with
            | WriteLocation (_, write_loc) ->
              if not (LocMap.mem write_loc node.write_locs) then
                node.write_locs <- LocMap.add write_loc prev_block_id node.write_locs
            | PhiChainJoin prev_node ->
              if prev_node <> node_id && not (IMap.mem prev_node node.prev_nodes) then
                node.prev_nodes <- IMap.add prev_node prev_block_id node.prev_nodes))
        new_sources
  in
  let phi_nodes_to_realize = ref IMap.empty in
  let rec visit_block ~sources ~prev_block_id block_id =
    let maybe_visit_block sources next_block_id =
      if ISet.mem next_block_id cx.visited_blocks then
        update_phis_from_sources next_block_id block_id sources
      else
        visit_block ~sources ~prev_block_id:block_id next_block_id
    in
    cx.visited_blocks <- ISet.add block_id cx.visited_blocks;
    update_phis_from_sources block_id prev_block_id sources;
    let sources = ref sources in
    (* Add phi chain join nodes as current sources *)
    begin
      match IMap.find_opt block_id cx.block_nodes with
      | None -> ()
      | Some decl_nodes ->
        LocMap.iter
          (fun decl_loc node_id -> sources := LocMap.add decl_loc (PhiChainJoin node_id) !sources)
          decl_nodes
    end;
    let block = IMap.find block_id program.blocks in
    List.iter (visit_instruction ~sources block_id) block.instructions;
    match block.next with
    | Halt -> ()
    | Continue continue -> maybe_visit_block !sources continue
    | Branch { test; continue; jump } ->
      visit_bool_value test sources;
      maybe_visit_block !sources continue;
      maybe_visit_block !sources jump
  and add_read read_loc sources =
    (* Save source for each use *)
    let decl_loc = Bindings.get_source_decl_loc_from_value_use pcx.Lex_analyze.bindings read_loc in
    let source = LocMap.find decl_loc !sources in
    cx.use_sources <- LocMap.add read_loc source cx.use_sources;
    (* If source is a phi node it should be realized *)
    match source with
    | WriteLocation _ -> ()
    | PhiChainJoin node_id ->
      phi_nodes_to_realize := IMap.add node_id decl_loc !phi_nodes_to_realize
  and add_write write_loc block_id sources =
    (* Source for this variable is now this write location *)
    let decl_loc = Bindings.get_source_decl_loc_from_value_use pcx.Lex_analyze.bindings write_loc in
    sources := LocMap.add decl_loc (WriteLocation (block_id, write_loc)) !sources;
    cx.write_var_ids <- LocMap.add write_loc (mk_var_id ()) cx.write_var_ids
  and visit_result v block_id sources =
    match v with
    | Id _ -> ()
    | Local loc -> add_write loc block_id sources
  and visit_var v sources =
    match v with
    | Id _ -> ()
    | Local loc -> add_read loc sources
  and visit_numeric_value v sources =
    let open Instruction.NumericValue in
    match v with
    | IntLit _ -> ()
    | IntVar var -> visit_var var sources
  and visit_bool_value v sources =
    let open Instruction.BoolValue in
    match v with
    | Lit _ -> ()
    | Var var -> visit_var var sources
  and visit_function_value v sources =
    let open Instruction.FunctionValue in
    match v with
    | Lit _ -> ()
    | Var var -> visit_var var sources
  and visit_value v sources =
    let open Instruction.Value in
    match v with
    | Unit Lit
    | String (Lit _) ->
      ()
    | Unit (Var var)
    | String (Var var) ->
      visit_var var sources
    | Bool bool -> visit_bool_value bool sources
    | Numeric numeric -> visit_numeric_value numeric sources
    | Function func -> visit_function_value func sources
  and visit_instruction ~sources block_id (_, instruction) =
    let open Instruction in
    let visit_result v = visit_result v block_id sources in
    let visit_numeric_value v = visit_numeric_value v sources in
    let visit_bool_value v = visit_bool_value v sources in
    let visit_function_value v = visit_function_value v sources in
    let visit_value v = visit_value v sources in
    match instruction with
    | Mov (result, arg) ->
      visit_value arg;
      visit_result result
    | Call (result, _, func, args) ->
      visit_function_value func;
      List.iter visit_value args;
      visit_result result
    | Ret arg -> Option.iter visit_value arg
    | LoadGlobal (result, _) -> visit_result result
    | StoreGlobal (_, arg) -> visit_value arg
    | Neg (result, arg) ->
      visit_numeric_value arg;
      visit_result result
    | LogNot (result, arg) ->
      visit_bool_value arg;
      visit_result result
    | LogAnd (result, left, right)
    | LogOr (result, left, right) ->
      visit_bool_value left;
      visit_bool_value right;
      visit_result result
    | Add (result, left, right)
    | Sub (result, left, right)
    | Mul (result, left, right)
    | Div (result, left, right)
    | Eq (result, left, right)
    | Neq (result, left, right)
    | Lt (result, left, right)
    | LtEq (result, left, right)
    | Gt (result, left, right)
    | GtEq (result, left, right) ->
      visit_numeric_value left;
      visit_numeric_value right;
      visit_result result
  in
  (* Visit bodies of all functions *)
  cx.visited_blocks <- ISet.empty;
  SMap.iter
    (fun _ { Function.params; body_start_block; _ } ->
      (* Set up sources for every param *)
      let sources =
        List.fold_left
          (fun sources (loc, var_id, _) ->
            cx.write_var_ids <- LocMap.add loc var_id cx.write_var_ids;
            LocMap.add loc (WriteLocation (body_start_block, loc)) sources)
          LocMap.empty
          params
      in
      visit_block ~sources ~prev_block_id:body_start_block body_start_block)
    program.funcs;
  (* To realize a phi node, that phi node and its entire phi chain graph should be realized *)
  let rec realize_phi_chain_graph node_id decl_loc =
    let node = get_node ~cx node_id in
    if Option.is_none node.realized then node.realized <- Some (mk_var_id ());
    add_realized_phi ~cx node.block_id decl_loc node_id;
    IMap.iter (fun prev_node_id _ -> realize_phi_chain_graph prev_node_id decl_loc) node.prev_nodes
  in
  IMap.iter realize_phi_chain_graph !phi_nodes_to_realize

and map_to_ssa ~pcx ~cx program =
  cx.visited_blocks <- ISet.empty;
  let rec visit_block block_id =
    cx.visited_blocks <- ISet.add block_id cx.visited_blocks;
    let maybe_visit_block next_block_id =
      if ISet.mem next_block_id cx.visited_blocks then
        ()
      else
        visit_block next_block_id
    in
    let block = IMap.find block_id program.blocks in
    let explicit_phis =
      List.map
        (fun (value_type, return, args) ->
          (value_type, map_write_var return, IMap.map map_read_var args))
        block.phis
    in
    let realized_phis =
      match IMap.find_opt block_id cx.realized_phis with
      | None -> []
      | Some realized_phis ->
        let phis =
          LocMap.fold
            (fun decl_loc node_id phis ->
              let node = get_node ~cx node_id in
              let value_type = Emit.value_type_of_decl_loc ~pcx decl_loc in
              let var_id = Option.get node.realized in
              let args = gather_phi_var_ids node_id in
              (value_type, var_id, args) :: phis)
            realized_phis
            []
        in
        List.rev phis
    in
    let instructions =
      List.map (fun (loc, instruction) -> (loc, map_instruction instruction)) block.instructions
    in
    let next =
      let open Block in
      match block.next with
      | Halt -> Halt
      | Continue continue ->
        maybe_visit_block continue;
        Continue continue
      | Branch { test; continue; jump } ->
        let test = map_bool_value ~f:map_read_var test in
        maybe_visit_block continue;
        maybe_visit_block jump;
        Branch { test; continue; jump }
    in
    let block =
      {
        Block.id = block_id;
        phis = explicit_phis @ realized_phis;
        instructions;
        next;
        source = block.source;
      }
    in
    cx.blocks <- IMap.add block.id block cx.blocks
  (* Traverse phi chain graph to gather all variable ids for a given phi node. The phi chain
     graph is deeply traversed until realized nodes are encountered. *)
  and gather_phi_var_ids (node_id : PhiChainNode.id) : Block.id IMap.t =
    let var_ids = ref IMap.empty in
    let add_var_id prev_block_id var_id = var_ids := IMap.add prev_block_id var_id !var_ids in
    let rec visit_phi_node node_id prev_block_id =
      let node = get_node ~cx node_id in
      match node.realized with
      (* Do not descend into realized nodes, use their realized var id *)
      | Some var_id -> add_var_id prev_block_id var_id
      (* Descend into unrealized nodes, gather local writes and phi chain nodes *)
      | _ ->
        LocMap.iter
          (fun write_loc _ -> add_var_id prev_block_id (write_var_id write_loc))
          node.write_locs;
        IMap.iter (fun node_id _ -> visit_phi_node node_id prev_block_id) node.prev_nodes
    in
    let node = get_node ~cx node_id in
    LocMap.iter
      (fun write_loc prev_block_id -> add_var_id prev_block_id (write_var_id write_loc))
      node.write_locs;
    IMap.iter visit_phi_node node.prev_nodes;
    !var_ids
  and write_var_id loc = LocMap.find loc cx.write_var_ids
  and map_read_var (cf_var : cf_var) : var_id =
    match cf_var with
    | Id var_id -> var_id
    | Local loc ->
      (match LocMap.find loc cx.use_sources with
      | WriteLocation (_, write_loc) -> write_var_id write_loc
      | PhiChainJoin node_id ->
        (* Phi chains which are sources must have been realized *)
        let node = get_node ~cx node_id in
        Option.get node.realized)
  and map_write_var cf_var =
    match cf_var with
    | Id var_id -> var_id
    | Local loc -> write_var_id loc
  and map_instruction instruction =
    let open Instruction in
    let map_return = map_write_var in
    let map_value = map_value ~f:map_read_var in
    let map_bool_value = map_bool_value ~f:map_read_var in
    let map_numeric_value = map_numeric_value ~f:map_read_var in
    let map_function_value = map_function_value ~f:map_read_var in
    match instruction with
    | Mov (return, arg) -> Mov (map_return return, map_value arg)
    | Call (return, ret_ty, func, args) ->
      Call (map_return return, ret_ty, map_function_value func, List.map map_value args)
    | Ret arg -> Ret (Option.map map_value arg)
    | LoadGlobal (return, global_loc) -> LoadGlobal (map_return return, global_loc)
    | StoreGlobal (global_loc, arg) -> StoreGlobal (global_loc, map_value arg)
    | LogNot (return, arg) -> LogNot (map_return return, map_bool_value arg)
    | LogAnd (return, left, right) ->
      LogAnd (map_return return, map_bool_value left, map_bool_value right)
    | LogOr (return, left, right) ->
      LogOr (map_return return, map_bool_value left, map_bool_value right)
    | Neg (return, arg) -> Neg (map_return return, map_numeric_value arg)
    | Add (return, left, right) ->
      Add (map_return return, map_numeric_value left, map_numeric_value right)
    | Sub (return, left, right) ->
      Sub (map_return return, map_numeric_value left, map_numeric_value right)
    | Mul (return, left, right) ->
      Mul (map_return return, map_numeric_value left, map_numeric_value right)
    | Div (return, left, right) ->
      Div (map_return return, map_numeric_value left, map_numeric_value right)
    | Eq (return, left, right) ->
      Eq (map_return return, map_numeric_value left, map_numeric_value right)
    | Neq (return, left, right) ->
      Neq (map_return return, map_numeric_value left, map_numeric_value right)
    | Lt (return, left, right) ->
      Lt (map_return return, map_numeric_value left, map_numeric_value right)
    | LtEq (return, left, right) ->
      LtEq (map_return return, map_numeric_value left, map_numeric_value right)
    | Gt (return, left, right) ->
      Gt (map_return return, map_numeric_value left, map_numeric_value right)
    | GtEq (return, left, right) ->
      GtEq (map_return return, map_numeric_value left, map_numeric_value right)
  in
  cx.visited_blocks <- ISet.empty;
  visit_block program.main_id;
  SMap.iter (fun _ { Global.init_start_block; _ } -> visit_block init_start_block) program.globals;
  SMap.iter (fun _ { Function.body_start_block; _ } -> visit_block body_start_block) program.funcs;
  (* Strip empty blocks *)
  IMap.iter
    (fun block_id { Block.phis; instructions; next; _ } ->
      match next with
      | Continue continue_id when instructions = [] && phis = [] ->
        (match IMap.find_opt block_id cx.prev_blocks with
        | None -> ()
        | Some prev_blocks when ISet.is_empty prev_blocks -> ()
        | Some prev_blocks ->
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
                  Branch { test; continue = map_next_id continue; jump = map_next_id jump }
              in
              block.next <- next')
            prev_blocks;
          (* Next node may have phis that point the removed block. Rewrite them to instead point to
             previous blocks. *)
          let next_node = IMap.find continue_id cx.blocks in
          next_node.phis <-
            List.map
              (fun (value_type, var_id, args) ->
                let args' =
                  match IMap.find_opt block_id args with
                  | None -> args
                  | Some arg_var_id ->
                    let args' = IMap.remove block_id args in
                    ISet.fold
                      (fun prev_block_id -> IMap.add prev_block_id arg_var_id)
                      prev_blocks
                      args'
                in
                (value_type, var_id, args'))
              next_node.phis;
          (* Remove this empty node *)
          cx.blocks <- IMap.remove block_id cx.blocks;
          cx.prev_blocks <- IMap.remove block_id cx.prev_blocks;
          cx.prev_blocks <-
            IMap.add
              continue_id
              (ISet.union prev_blocks (IMap.find continue_id cx.prev_blocks))
              cx.prev_blocks)
      | _ -> ())
    cx.blocks;
  let globals =
    SMap.map
      (fun global ->
        let open Global in
        { global with init_val = map_value ~f:map_read_var global.init_val })
      program.globals
  in
  {
    Program.main_id = program.main_id;
    blocks = cx.blocks;
    globals;
    funcs = program.funcs;
    modules = program.modules;
  }
