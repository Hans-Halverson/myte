open Basic_collections
open X86_gen_context
open X86_instructions

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

class liveness_init_visitor (blocks_by_id : virtual_block IMap.t) =
  object (this)
    val mutable prev_blocks =
      IMap.fold (fun block_id _ acc -> IMap.add block_id ISet.empty acc) blocks_by_id IMap.empty

    val mutable vreg_use_blocks = IMap.empty

    val mutable vreg_def_blocks = IMap.empty

    val mutable vreg_use_before_def_blocks = IMap.empty

    method prev_blocks = prev_blocks

    method vreg_use_blocks = vreg_use_blocks

    method vreg_def_blocks = vreg_def_blocks

    method vreg_use_before_def_blocks = vreg_use_before_def_blocks

    method add_edge block_id1 block_id2 =
      prev_blocks <-
        IMap.add block_id2 (ISet.add block_id1 (IMap.find block_id2 prev_blocks)) prev_blocks

    method run () = IMap.iter (fun _ block -> this#visit_block block) blocks_by_id

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method visit_instruction ~block instr =
      let (_, instr) = instr in
      match instr with
      | PushR read_vreg
      | IDivR read_vreg
      | CallR read_vreg ->
        this#visit_read_vreg ~block read_vreg
      | PopR write_vreg -> this#visit_write_vreg ~block write_vreg
      | NegR read_write_vreg
      | NotR read_write_vreg ->
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | PushM mem
      | CallM mem ->
        this#visit_memory_address ~block mem
      | CmpRR (read_vreg1, read_vreg2)
      | TestRR (read_vreg1, read_vreg2) ->
        this#visit_read_vreg ~block read_vreg1;
        this#visit_read_vreg ~block read_vreg2
      | MovRR (read_vreg, write_vreg)
      | IMulRIR (read_vreg, _, write_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_write_vreg ~block write_vreg
      | AddRR (read_vreg, read_write_vreg)
      | SubRR (read_vreg, read_write_vreg)
      | IMulRR (read_vreg, read_write_vreg)
      | AndRR (read_vreg, read_write_vreg)
      | OrRR (read_vreg, read_write_vreg) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | MovMR (mem, write_vreg)
      | Lea (mem, write_vreg) ->
        this#visit_memory_address ~block mem;
        this#visit_write_vreg ~block write_vreg
      | MovRM (read_vreg, mem) ->
        this#visit_read_vreg ~block read_vreg;
        this#visit_memory_address ~block mem
      | MovIR (_, write_vreg)
      | SetCmp (_, write_vreg) ->
        this#visit_write_vreg ~block write_vreg
      | AddIR (_, read_write_vreg)
      | SubIR (_, read_write_vreg)
      | AndIR (_, read_write_vreg)
      | OrIR (_, read_write_vreg) ->
        this#visit_read_vreg ~block read_write_vreg;
        this#visit_write_vreg ~block read_write_vreg
      | MovIM (_, mem) -> this#visit_memory_address ~block mem
      | CmpRI (read_vreg, _) -> this#visit_read_vreg ~block read_vreg
      | Jmp next_block_id
      | CondJmp (_, next_block_id) ->
        this#add_edge block.id next_block_id
      | PushI _
      | Leave
      | Ret
      | Syscall ->
        ()

    method visit_memory_address ~block mem =
      begin
        match mem.base with
        | IP -> ()
        | BaseRegister vreg -> this#visit_read_vreg ~block vreg
      end;
      match mem.index_and_scale with
      | None -> ()
      | Some (index_vreg, _) -> this#visit_read_vreg ~block index_vreg

    method visit_read_vreg ~block vreg_id =
      vreg_use_blocks <- add_to_multimap vreg_id block.id vreg_use_blocks

    method visit_write_vreg ~block vreg_id =
      if
        in_multimap vreg_id block.id vreg_use_blocks
        && not (in_multimap vreg_id block.id vreg_def_blocks)
      then
        vreg_use_before_def_blocks <- add_to_multimap vreg_id block.id vreg_use_before_def_blocks;
      vreg_def_blocks <- add_to_multimap vreg_id block.id vreg_def_blocks
  end

let rec liveness_analysis ~(gcx : Gcx.t) = liveness_analysis_from_blocks gcx.blocks_by_id

and liveness_analysis_from_blocks blocks_by_id =
  (* Calculate use and def blocks for each variable *)
  let init_visitor = new liveness_init_visitor blocks_by_id in
  init_visitor#run ();

  let prev_blocks = init_visitor#prev_blocks in
  let vreg_use_blocks = init_visitor#vreg_use_blocks in
  let vreg_def_blocks = init_visitor#vreg_def_blocks in
  let vreg_use_before_def_blocks = init_visitor#vreg_use_before_def_blocks in

  (* Initialize liveness sets *)
  let live_in = ref IMap.empty in
  let live_out = ref IMap.empty in
  IMap.iter
    (fun block_id _ ->
      live_in := IMap.add block_id [] !live_in;
      live_out := IMap.add block_id [] !live_out)
    blocks_by_id;

  (* Propagate a single variable backwards through the program, building liveness sets as we go *)
  let set_contains set block_id var_id =
    match IMap.find block_id !set with
    | hd :: _ when hd = var_id -> true
    | _ -> false
  in
  let set_add set block_id var_id =
    set := IMap.add block_id (var_id :: IMap.find block_id !set) !set
  in
  let rec propagate_backwards ~block_id ~vreg_id =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       vreg is defined (unless the vreg is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block_id vreg_id))
      && ( (not (in_multimap vreg_id block_id vreg_def_blocks))
         || in_multimap vreg_id block_id vreg_use_before_def_blocks )
    then (
      set_add live_in block_id vreg_id;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block vreg_id) then set_add live_out prev_block vreg_id;
          propagate_backwards ~block_id:prev_block ~vreg_id)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  IMap.iter
    (fun vreg_id use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~vreg_id) use_blocks)
    vreg_use_blocks;

  (!live_in, !live_out)
