open Basic_collections
open Myte_test
open X86_64_builders
open X86_64_instructions
open X86_64_register

let mk_vreg id = mk_virtual_register_of_value_id ~value_id:id ~type_:Byte

let mk_blocks blocks =
  List.fold_left
    (fun blocks (id, label, instructions) ->
      {
        Block.id;
        label = Some label;
        func = null_function;
        instructions =
          List.map (fun instr -> Instruction.{ id = mk_instr_id (); instr }) instructions;
      }
      :: blocks)
    []
    blocks

let find_liveness_sets blocks =
  let open Operand in
  let blocks_by_id = mk_blocks blocks in
  let (live_in, live_out) = X86_64_liveness_analysis.analyze_regs blocks_by_id RegMap.empty in

  ( BlockMap.fold
      (fun block set_as_list acc ->
        IMap.add
          block.id
          (List.fold_left (fun acc v -> ISet.add v.id acc) ISet.empty set_as_list)
          acc)
      live_in
      IMap.empty,
    BlockMap.fold
      (fun block set_as_list acc ->
        IMap.add
          block.id
          (List.fold_left (fun acc v -> ISet.add v.id acc) ISet.empty set_as_list)
          acc)
      live_out
      IMap.empty )

let assert_liveness_sets (all_live_in, all_live_out) block_id ~live_in ~live_out =
  assert_iset_equals (IMap.find block_id all_live_in) live_in;
  assert_iset_equals (IMap.find block_id all_live_out) live_out

let tests =
  let open Instruction in
  let block0 = mk_block ~func:null_function in
  let block1 = mk_block ~func:null_function in
  let block2 = mk_block ~func:null_function in
  let block3 = mk_block ~func:null_function in
  let block4 = mk_block ~func:null_function in
  [
    ( "use_without_def",
      (* If there is no def for a use the vreg is live in (and assumed to have come from another
         source such as function parameters. *)
      fun _ ->
        let sets = find_liveness_sets [(0, "start", [PushM (mk_vreg 1); Ret])] in
        assert_liveness_sets sets 0 ~live_in:[1] ~live_out:[] );
    ( "use_stops_propagation",
      fun _ ->
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopM (mk_vreg 0); PopM (mk_vreg 1); Jmp block1]);
              (1, "L1", [PushM (mk_vreg 0); Jmp block2]);
              (2, "L2", [PushM (mk_vreg 1); Jmp block3]);
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
            [
              (0, "start", [PopM (mk_vreg 0); PushM (mk_vreg 0); Jmp block1]);
              (1, "L1", [PopM (mk_vreg 1); PushM (mk_vreg 1); Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[];
        assert_liveness_sets sets 1 ~live_in:[] ~live_out:[] );
    ( "use_before_def_in_block",
      fun _ ->
        (* A use before a def of the same vreg in the same block should still be live in *)
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopM (mk_vreg 0); Jmp block1]);
              (1, "L1", [PushM (mk_vreg 0); PopM (mk_vreg 0); Jmp block2]);
              (2, "L2", [PushM (mk_vreg 0); Ret]);
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
              ( 0,
                "start",
                [
                  PopM (mk_vreg 0);
                  PopM (mk_vreg 1);
                  PopM (mk_vreg 2);
                  PopM (mk_vreg 3);
                  JmpCC (E, block1);
                  Jmp block2;
                ] );
              (1, "L1", [PushM (mk_vreg 0); PushM (mk_vreg 3); Jmp block3]);
              (2, "L2", [PushM (mk_vreg 1); Jmp block3]);
              (3, "L3", [PushM (mk_vreg 2); PushM (mk_vreg 3); Ret]);
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
              (0, "start", [PopM (mk_vreg 0); PopM (mk_vreg 1); JmpCC (E, block1); Jmp block3]);
              (1, "L1", [PushM (mk_vreg 0); Jmp block2]);
              (2, "L2", [PushM (mk_vreg 1); Jmp block0]);
              (3, "L3", [PushM (mk_vreg 0); Ret]);
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
              (0, "start", [PopM (mk_vreg 0); PopM (mk_vreg 1); Jmp block1]);
              (1, "L1", [PushM (mk_vreg 0); JmpCC (E, block2); Jmp block4]);
              (2, "L2", [Jmp block3]);
              (3, "L3", [PopM (mk_vreg 1); Jmp block1]);
              (4, "L4", [PushM (mk_vreg 1); Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0; 1];
        assert_liveness_sets sets 1 ~live_in:[0; 1] ~live_out:[0; 1];
        assert_liveness_sets sets 2 ~live_in:[0] ~live_out:[0];
        assert_liveness_sets sets 3 ~live_in:[0] ~live_out:[0; 1];
        assert_liveness_sets sets 4 ~live_in:[1] ~live_out:[] );
    ( "self_xor_is_not_use",
      fun _ ->
        let sets =
          find_liveness_sets
            [
              (0, "start", [PopM (mk_vreg 0); PopM (mk_vreg 1); PopM (mk_vreg 2); Jmp block1]);
              ( 1,
                "L1",
                [
                  XorMM (Size64, mk_vreg 1, mk_vreg 1);
                  XorMM (Size64, mk_vreg 0, mk_vreg 2);
                  Jmp block2;
                ] );
              (2, "L2", [PushM (mk_vreg 1); PushM (mk_vreg 2); Ret]);
            ]
        in
        assert_liveness_sets sets 0 ~live_in:[] ~live_out:[0; 2];
        assert_liveness_sets sets 1 ~live_in:[0; 2] ~live_out:[1; 2];
        assert_liveness_sets sets 2 ~live_in:[1; 2] ~live_out:[] );
  ]

let suite =
  {
    Suite.name = "liveness_analysis";
    suites = [];
    tests = List.map (fun (name, f) -> { Test.name; run = run_unit_test f }) tests;
  }
