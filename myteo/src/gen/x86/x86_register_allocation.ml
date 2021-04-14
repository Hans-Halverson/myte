open Basic_collections
open Mir
module Ocx = Mir_optimize_context

let add_to_multimap key value mmap =
  let new_values =
    match IMap.find_opt key mmap with
    | None -> ISet.singleton value
    | Some values -> ISet.add value values
  in
  IMap.add key new_values mmap

let in_multimap key value mmap =
  match IMap.find_opt key mmap with
  | None -> false
  | Some values -> ISet.mem value values

class liveness_init_visitor ~program =
  object
    inherit Ocx.IRVisitor.t ~program

    val mutable prev_blocks =
      IMap.fold (fun block_id _ acc -> IMap.add block_id ISet.empty acc) program.blocks IMap.empty

    val mutable var_def_blocks = IMap.empty

    val mutable var_use_blocks = IMap.empty

    method prev_blocks = prev_blocks

    method var_def_blocks = var_def_blocks

    method var_use_blocks = var_use_blocks

    method! visit_edge b1 b2 =
      prev_blocks <- IMap.add b2.id (ISet.add b1.id (IMap.find b2.id prev_blocks)) prev_blocks

    method! visit_result_variable ~block ~instruction:_ var_id =
      var_def_blocks <- add_to_multimap var_id block.id var_def_blocks

    method! visit_use_variable ~block var_id =
      var_use_blocks <- add_to_multimap var_id block.id var_use_blocks
  end

let liveness_analysis (ir : var_id Program.t) =
  (* Calculate use and def blocks for each variable *)
  let init_visitor = new liveness_init_visitor ~program:ir in
  init_visitor#run ();

  let prev_blocks = init_visitor#prev_blocks in
  let var_def_blocks = init_visitor#var_def_blocks in
  let var_use_blocks = init_visitor#var_use_blocks in

  (* Initialize liveness sets *)
  let live_in = ref IMap.empty in
  let live_out = ref IMap.empty in
  IMap.iter
    (fun block_id _ ->
      live_in := IMap.add block_id [] !live_in;
      live_out := IMap.add block_id [] !live_out)
    ir.blocks;

  (* Propagate a single variable backwards through the program, building liveness sets as we go *)
  let set_contains set block_id var_id =
    match IMap.find block_id !set with
    | hd :: _ when hd = var_id -> true
    | _ -> false
  in
  let set_add set block_id var_id =
    set := IMap.add block_id (var_id :: IMap.find block_id !set) !set
  in
  let rec propagate_backwards ~block_id ~var_id =
    if
      (not (in_multimap var_id block_id var_def_blocks))
      && not (set_contains live_in block_id var_id)
    then (
      set_add live_in block_id var_id;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block var_id) then set_add live_out prev_block var_id;
          propagate_backwards ~block_id:prev_block ~var_id)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  IMap.iter
    (fun var_id use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~var_id) use_blocks)
    var_use_blocks;

  (!live_in, !live_out)
