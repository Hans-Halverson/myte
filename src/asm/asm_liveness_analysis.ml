open Asm
open Asm_builders

class virtual liveness_analyzer (func : Function.t) =
  object (this)
    method virtual prev_blocks : BlockMMap.t
    method virtual use_blocks : OBMMap.t
    method virtual def_blocks : OBMMap.t
    method virtual use_before_def_blocks : OBMMap.t

    method virtual init : unit -> unit

    method analyze () =
      (* Calculate use and def blocks for each item *)
      this#init ();

      let prev_blocks = this#prev_blocks in
      let use_blocks = this#use_blocks in
      let def_blocks = this#def_blocks in
      let use_before_def_blocks = this#use_before_def_blocks in

      (* Initialize liveness sets *)
      let live_in = ref BlockMap.empty in
      let live_out = ref BlockMap.empty in
      func_iter_blocks func (fun block ->
          live_in := BlockMap.add block [] !live_in;
          live_out := BlockMap.add block [] !live_out);

      (* Propagate a single variable backwards through the program, building liveness sets as we go *)
      let set_contains set block item =
        match BlockMap.find block !set with
        | hd :: _ when hd == item -> true
        | _ -> false
      in
      let set_add set block item =
        set := BlockMap.add block (item :: BlockMap.find block !set) !set
      in
      let rec propagate_backwards ~block ~item =
        (* Stop backwards propagation if we reach a block that has already been visited or where the
           var is defined (unless the var is used in the block before it is defined in the block) *)
        if
          (not (set_contains live_in block item))
          && ((not (OBMMap.contains item block def_blocks))
             || OBMMap.contains item block use_before_def_blocks)
        then (
          set_add live_in block item;
          let prev_blocks = BlockMMap.find_all block prev_blocks in
          BlockSet.iter
            (fun prev_block ->
              if not (set_contains live_out prev_block item) then set_add live_out prev_block item;
              propagate_backwards ~block:prev_block ~item)
            prev_blocks
        )
      in

      (* Liveness is calculated for all variables in program *)
      OBMMap.iter
        (fun item use_blocks ->
          BlockSet.iter (fun block -> propagate_backwards ~block ~item) use_blocks)
        use_blocks;

      (!live_in, !live_out)
  end
