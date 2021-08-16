open Basic_collections
open Mir

(* SSA destruction - remove all phi nodes in program and replace with explicit move instructions.
   Moves are inserted in the previous block, unless the previous block branches, in which case the
   edge is split and moves are inserted in a new block between the previous and current blocks.
   
   Note that the semantics of phi nodes are that all phi nodes execute in parallel. This means that
   all lowered copy instructions happen in parallel as well to preserve semantics. We must convert
   the parallel copies to a linear sequence of copies, which may need to introduce new copies with
   temporary variables in the presence of cyclic copies. *)

(* Split edges where Mov instructions will need to be created. Edges are split by inserting an empty
   block between them, which will be filled with Mov instructions later. *)
let split_edges ~(ir : Program.t) =
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
    !edges_to_split

(* Convert a collection of copies (from variables to values) that should that occur in parallel, to
   an explicit sequence of copies, making sure to preserve semantics. In the presence of cycles, new
   variables with new copies will be introduced to break the cycles while not clobbering any vars. *)
let sequentialize_parallel_copies (parallel_copies : (var_id * Value.t) list) =
  let copy_sequence = ref [] in
  let add_to_sequence result_var_id arg_val =
    copy_sequence := (result_var_id, arg_val) :: !copy_sequence
  in

  (* Map from arg var id to its type *)
  let var_types = ref IMap.empty in

  (* Copy graph between variables *)
  let copied_to = ref IIMMap.empty in
  let copied_from = ref IIMMap.empty in

  let add_copy_edge result_var_id arg_var_id =
    copied_to := IIMMap.add arg_var_id result_var_id !copied_to;
    copied_from := IIMMap.add result_var_id arg_var_id !copied_from
  in
  let remove_copy_edge result_var_id arg_var_id =
    copied_to := IIMMap.remove arg_var_id result_var_id !copied_to;
    copied_from := IIMMap.remove result_var_id arg_var_id !copied_from
  in

  (* Initialize copy graph *)
  List.iter
    (fun (result_var_id, arg) ->
      match var_id_of_value_opt arg with
      (* Copies of literal values can always be immediately sequentialized, as they cannot form cycles *)
      | None -> add_to_sequence result_var_id arg
      | Some arg_var_id ->
        (* Add edge to graph for copy between variables. Self copies can be ignored *)
        if arg_var_id <> result_var_id then (
          add_copy_edge result_var_id arg_var_id;
          var_types := IMap.add arg_var_id (type_of_value arg) !var_types
        ))
    parallel_copies;

  while not (IIMMap.is_empty !copied_to) do
    (* Try to find a copy that is not part of a cycle *)
    let non_cyclic_copy_opt =
      IMap.fold
        (fun result_var_id arg_var_ids acc ->
          if IIMMap.contains_key result_var_id !copied_to then
            acc
          else
            Some (result_var_id, ISet.choose arg_var_ids))
        !copied_from
        None
    in
    match non_cyclic_copy_opt with
    (* Non-cyclic copy can now be added to sequence, as it has no dependencies *)
    | Some (result_var_id, arg_var_id) ->
      let var_type = IMap.find arg_var_id !var_types in
      add_to_sequence result_var_id (var_value_of_type arg_var_id var_type);
      remove_copy_edge result_var_id arg_var_id
    (* Only cyclic copies exist - choose one and break it. Break by creating a new variable, copying
       the chosen arg var to it, and modifying the existing edge from that arg to its result to
       instead point from the new var to the result. *)
    | None ->
      let (result_var_id, arg_var_id) = IIMMap.choose !copied_from in
      let var_type = IMap.find arg_var_id !var_types in
      let new_var_id = mk_var_id () in
      var_types := IMap.add new_var_id var_type !var_types;
      add_to_sequence new_var_id (var_value_of_type arg_var_id var_type);
      remove_copy_edge result_var_id arg_var_id;
      add_copy_edge result_var_id new_var_id
  done;

  List.rev !copy_sequence

(* Remove phis, and replace with explicit move instructions in the previous block *)
let lower_phis_to_copies ~(ir : Program.t) =
  IMap.iter
    (fun _ block ->
      (* Collect all copies to create from phi nodes in each previous block *)
      let block_to_parallel_copies =
        List.fold_left
          (fun acc (_, var_id, args) ->
            IMap.fold
              (fun prev_block_id arg_val acc ->
                let existing_copies = IMap.find_opt prev_block_id acc |> Option.value ~default:[] in
                IMap.add prev_block_id ((var_id, arg_val) :: existing_copies) acc)
              args
              acc)
          IMap.empty
          block.Block.phis
      in

      (* Create sequence for copies and emit Mov instructions in previous block *)
      IMap.iter
        (fun prev_block_id parallel_copies ->
          let sequential_copies = sequentialize_parallel_copies (List.rev parallel_copies) in
          let copy_instrs =
            List.map
              (fun (result_var_id, arg_val) ->
                (mk_instr_id (), Instruction.Mov (result_var_id, arg_val)))
              sequential_copies
          in
          let prev_block = IMap.find prev_block_id ir.blocks in
          prev_block.instructions <- prev_block.instructions @ copy_instrs)
        block_to_parallel_copies;

      block.Block.phis <- [])
    ir.blocks

let destruct_ssa (ir : Program.t) : Program.t =
  split_edges ~ir;
  lower_phis_to_copies ~ir;
  ir
