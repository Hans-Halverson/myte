open Mir
open Mir_builders

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
  let edges_to_split = ref BlockMMap.empty in

  (* Mark all edges that should be split. An edge from block A to block B should be split if
     block B contains a phi node that references A, but block A branches at the end. *)
  program_iter_blocks ir (fun block ->
      block_iter_phis block (fun _ { args; _ } ->
          BlockMap.iter
            (fun prev_block _ ->
              match get_terminator prev_block with
              | Some { instr = Branch _; _ } ->
                edges_to_split := BlockMMap.add block prev_block !edges_to_split
              | _ -> ())
            args));

  (* Split all marked edges *)
  BlockMMap.iter
    (fun block prev_blocks ->
      (* Create and insert new blocks, keeping map from previous block to new inserted block *)
      BlockSet.iter (fun prev_block -> ignore (split_block_edge prev_block block)) prev_blocks)
    !edges_to_split

(* Convert a collection of copies (dest value, src value) that should that occur in parallel, to
   an explicit sequence of copies, making sure to preserve semantics. In the presence of cycles, new
   variables with new copies will be introduced to break the cycles while not clobbering any vars. *)
let sequentialize_parallel_copies (parallel_copies : (Value.t * Value.t) list) :
    (Value.t * Value.t) list =
  let copy_sequence : (Value.t * Value.t) list ref = ref [] in
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
    (fun (dest_instr, src_instr) ->
      match src_instr.Value.value with
      | Value.Instr _
      | Argument _ ->
        (* Add edge to graph for copy between variables. Self copies can be ignored *)
        if Value.(src_instr.id <> dest_instr.id) then add_copy_edge dest_instr src_instr
      (* Copies of literal values can always be immediately sequentialized, as they cannot form cycles *)
      | Lit _ -> add_to_sequence dest_instr src_instr)
    parallel_copies;

  while not (VVMMap.is_empty !copied_to) do
    (* Try to find a copy that is not part of a cycle *)
    let non_cyclic_copy_opt =
      VVMMap.KMap.fold
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
      let new_arg_val = mk_blockless_mov ~arg:arg_val in
      add_to_sequence new_arg_val arg_val;
      remove_copy_edge dest_val arg_val;
      add_copy_edge dest_val new_arg_val
  done;

  List.rev !copy_sequence

(* Remove phis, and replace with explicit move instructions in the previous block *)
let lower_phis_to_copies ~(ir : Program.t) =
  program_iter_blocks ir (fun block ->
      (* Collect all copies to create from phi nodes in each previous block *)
      let block_to_parallel_copies =
        block_fold_phis block BlockMap.empty (fun instr_val { args } acc ->
            BlockMap.fold
              (fun prev_block arg_val acc ->
                let existing_copies =
                  BlockMap.find_opt prev_block acc |> Option.value ~default:[]
                in
                BlockMap.add prev_block ((instr_val, arg_val.Use.value) :: existing_copies) acc)
              args
              acc)
      in

      (* Create sequence for copies and emit Mov instructions in previous block *)
      BlockMap.iter
        (fun prev_block parallel_copies ->
          let sequential_copies = sequentialize_parallel_copies (List.rev parallel_copies) in
          let terminator_val = get_terminator_value prev_block |> Option.get in
          List.iter
            (fun (dest_val, arg_val) ->
              (* Destination instructions are all pre-existing Phis or Movs that were created
                 during sequentialization. This breaks references but preserves value ids, so
                 after this point arg/use references cannot be followed. *)
              let dest_instr = cast_to_instruction dest_val in
              let arg_use = user_add_use ~user:dest_val ~use:arg_val in
              let instr_val = { dest_val with value = null_value_value } in
              instr_val.value <-
                Instr { dest_instr with instr = Mov arg_use; next = instr_val; prev = instr_val };
              insert_instruction_before ~before:terminator_val instr_val)
            sequential_copies)
        block_to_parallel_copies;

      block_clear_phis block)

let run ~(program : Program.t) =
  split_edges ~ir:program;
  lower_phis_to_copies ~ir:program
