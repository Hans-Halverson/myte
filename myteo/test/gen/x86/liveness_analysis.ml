open Basic_collections
open Myte_test
open X86_instructions

let mk_blocks blocks =
  List.fold_left
    (fun blocks (id, label, instructions) ->
      IMap.add
        id
        {
          Block.id;
          label;
          instructions = List.map (fun instr -> (Instruction.mk_id (), instr)) instructions;
        }
        blocks)
    IMap.empty
    blocks

let find_liveness_sets blocks =
  let blocks_by_id = mk_blocks blocks in
  let (live_in, live_out) = X86_register_allocation.liveness_analysis_from_blocks blocks_by_id in
  ( IMap.map (fun set_as_list -> ISet.of_list set_as_list) live_in,
    IMap.map (fun set_as_list -> ISet.of_list set_as_list) live_out )

let assert_liveness_sets (all_live_in, all_live_out) block_id ~live_in ~live_out =
  assert_iset_equals (IMap.find block_id all_live_in) live_in;
  assert_iset_equals (IMap.find block_id all_live_out) live_out

let tests =
  let open Instruction in
  [
    ( "use_without_def",
      (* If there is no def for a use the vreg is live in (and assumed to have come from another
       source such as function parameters. *)
      fun _ ->
        let sets = find_liveness_sets [(0, "start", [PushR 1; Ret])] in
        assert_liveness_sets sets 0 ~live_in:[1] ~live_out:[] );
    ( "use_stops_propagation",
      fun _ ->
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopR 0; PopR 1; Jmp 1]);
              (1, "L1", [PushR 0; Jmp 2]);
              (2, "L2", [PushR 1; Jmp 3]);
              (3, "L3", [Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0; 1];
        assert_liveness_sets sets 1 ~live_in:[0; 1] ~live_out:[1];
        assert_liveness_sets sets 2 ~live_in:[1] ~live_out:[];
        assert_liveness_sets sets 3 ~live_in:[] ~live_out:[] );
    ( "use_after_def_in_block",
      fun _ ->
        let sets =
          find_liveness_sets
            [(0, "start", [PopR 0; PushR 0; Jmp 1]); (1, "L1", [PopR 1; PushR 1; Ret])]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[];
        assert_liveness_sets sets 1 ~live_in:[] ~live_out:[] );
    ( "use_before_def_in_block",
      fun _ ->
        (* A use before a def of the same vreg in the same block should still be live in *)
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopR 0; Jmp 1]);
              (1, "L1", [PushR 0; PopR 0; Jmp 2]);
              (2, "L2", [PushR 0; Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0];
        assert_liveness_sets sets 1 ~live_in:[0] ~live_out:[0];
        assert_liveness_sets sets 2 ~live_in:[0] ~live_out:[] );
    ( "liveness_propagates_down_branches",
      fun _ ->
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopR 0; PopR 1; PopR 2; PopR 3; CondJmp (Equal, 1); Jmp 2]);
              (1, "L1", [PushR 0; PushR 3; Jmp 3]);
              (2, "L2", [PushR 1; Jmp 3]);
              (3, "L3", [PushR 2; PushR 3; Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0; 1; 2; 3];
        assert_liveness_sets sets 1 ~live_in:[0; 2; 3] ~live_out:[2; 3];
        assert_liveness_sets sets 2 ~live_in:[1; 2; 3] ~live_out:[2; 3];
        assert_liveness_sets sets 3 ~live_in:[2; 3] ~live_out:[] );
    ( "loop_with_partial_liveness",
      fun _ ->
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopR 0; PopR 1; CondJmp (Equal, 1); Jmp 3]);
              (1, "L1", [PushR 0; Jmp 2]);
              (2, "L2", [PushR 1; Jmp 0]);
              (3, "L3", [PushR 0; Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0; 1];
        assert_liveness_sets sets 1 ~live_in:[0; 1] ~live_out:[1];
        assert_liveness_sets sets 2 ~live_in:[1] ~live_out:[];
        assert_liveness_sets sets 3 ~live_in:[0] ~live_out:[] );
    ( "loop_with_full_liveness",
      fun _ ->
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopR 0; PopR 1; Jmp 1]);
              (1, "L1", [PushR 0; CondJmp (Equal, 2); Jmp 4]);
              (2, "L2", [Jmp 3]);
              (3, "L3", [PopR 1; Jmp 1]);
              (4, "L4", [PushR 1; Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0; 1];
        assert_liveness_sets sets 1 ~live_in:[0; 1] ~live_out:[0; 1];
        assert_liveness_sets sets 2 ~live_in:[0] ~live_out:[0];
        assert_liveness_sets sets 3 ~live_in:[0] ~live_out:[0; 1];
        assert_liveness_sets sets 4 ~live_in:[1] ~live_out:[] );
  ]

let suite =
  {
    Suite.name = "liveness_analysis";
    suites = [];
    tests = List.map (fun (name, f) -> { Test.name; run = run_unit_test f }) tests;
  }
