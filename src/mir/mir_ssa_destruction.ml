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
      block_iter_phis block (fun { args; _ } ->
          IMap.iter
            (fun prev_block_id _ ->
              let prev_block = IMap.find prev_block_id ir.blocks in
              match prev_block.next with
              | Branch _ -> mark_edge_to_split prev_block_id block.id
              | _ -> ())
            args))
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
                  instructions = None;
                  next = Continue block;
                }
              in
              ir.blocks <- IMap.add new_block_id new_block ir.blocks;
              let map_next_block next_block =
                if next_block == block then
                  new_block
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
      block_iter_phis block (fun ({ args; _ } as phi) ->
          phi.args <-
            IMap.fold
              (fun prev_block_id arg args' ->
                match IMap.find_opt prev_block_id prev_to_new_block with
                | None -> IMap.add prev_block_id arg args'
                | Some new_block_id -> IMap.add new_block_id arg args')
              args
              IMap.empty))
    !edges_to_split

(* Convert a collection of copies (dest value, src value) that should that occur in parallel, to
   an explicit sequence of copies, making sure to preserve semantics. In the presence of cycles, new
   variables with new copies will be introduced to break the cycles while not clobbering any vars. *)
let sequentialize_parallel_copies (parallel_copies : (Value.t * Value.t) list) :
    (Value.t * Value.t) list =
  let copy_sequence = ref [] in
  let add_to_sequence dest_val src_val = copy_sequence := (dest_val, src_val) :: !copy_sequence in

  (* Copy graph between values *)
  let copied_to = ref VVMMap.empty in
  let copied_from = ref VVMMap.empty in

  let add_copy_edge dest_val arg_val =
    copied_to := VVMMap.add arg_val dest_val !copied_to;
    copied_from := VVMMap.add dest_val arg_val !copied_from
  in
  let remove_copy_edge dest_val arg_val =
    copied_to := VVMMap.remove arg_val dest_val !copied_to;
    copied_from := VVMMap.remove dest_val arg_val !copied_from
  in

  (* Initialize copy graph *)
  List.iter
    (fun (dest_val, src_val) ->
      let dest_instr = cast_to_instruction dest_val in
      match src_val with
      | Value.Instr { id; _ }
      | Argument { id; _ } ->
        (* Add edge to graph for copy between variables. Self copies can be ignored *)
        if id <> dest_instr.id then add_copy_edge dest_val src_val
      (* Copies of literal values can always be immediately sequentialized, as they cannot form cycles *)
      | Lit _ -> add_to_sequence dest_val src_val)
    parallel_copies;

  while not (VVMMap.is_empty !copied_to) do
    (* Try to find a copy that is not part of a cycle *)
    let non_cyclic_copy_opt =
      VMap.fold
        (fun dest_val arg_vals acc ->
          if VVMMap.contains_key dest_val !copied_to then
            acc
          else
            Some (dest_val, VSet.choose arg_vals))
        !copied_from
        None
    in
    match non_cyclic_copy_opt with
    (* Non-cyclic copy can now be added to sequence, as it has no dependencies *)
    | Some (dest_val, arg_val) ->
      add_to_sequence dest_val arg_val;
      remove_copy_edge dest_val arg_val
    (* Only cyclic copies exist - choose one and break it. Break by creating a new variable, copying
       the chosen arg var to it, and modifying the existing edge from that arg to its result to
       instead point from the new var to the result. *)
    | None ->
      let (dest_val, arg_val) = VVMMap.choose !copied_from in
      let new_arg_val = Value.Instr (Mir_builders.mk_mov ~arg:arg_val) in
      add_to_sequence new_arg_val arg_val;
      remove_copy_edge dest_val arg_val;
      add_copy_edge dest_val new_arg_val
  done;

  List.rev !copy_sequence

(* Remove phis, and replace with explicit move instructions in the previous block *)
let lower_phis_to_copies ~(ir : Program.t) =
  IMap.iter
    (fun _ block ->
      (* Collect all copies to create from phi nodes in each previous block *)
      let block_to_parallel_copies =
        block_fold_phis block IMap.empty (fun instr { args } acc ->
            IMap.fold
              (fun prev_block_id arg_val acc ->
                let existing_copies = IMap.find_opt prev_block_id acc |> Option.value ~default:[] in
                IMap.add prev_block_id ((Value.Instr instr, arg_val) :: existing_copies) acc)
              args
              acc)
      in

      (* Create sequence for copies and emit Mov instructions in previous block *)
      IMap.iter
        (fun prev_block_id parallel_copies ->
          let prev_block = IMap.find prev_block_id ir.blocks in
          let sequential_copies = sequentialize_parallel_copies (List.rev parallel_copies) in
          List.iter
            (fun (dest_val, arg_val) ->
              (* Destination instructions are all pre-existing Phis or Movs that were created
                 during sequentialization. This breaks references but preserves value ids, so
                 after this point arg/use references cannot be followed. *)
              let dest_instr = cast_to_instruction dest_val in
              let rec instr = { dest_instr with instr = Mov arg_val; next = instr; prev = instr } in
              append_instruction prev_block instr)
            sequential_copies)
        block_to_parallel_copies;

      block_clear_phis block)
    ir.blocks

let destruct_ssa (ir : Program.t) : Program.t =
  split_edges ~ir;
  lower_phis_to_copies ~ir;
  ir
