type opts = {
  bin: string ref;
  filter: string ref;
  record: bool ref;
  tree: bool ref;
  tree_full: bool ref;
}

let default_bin = Filename.concat (Sys.getcwd ()) "_build/default/src/myte.exe"

let opts =
  {
    bin = ref default_bin;
    filter = ref "";
    record = ref false;
    tree = ref false;
    tree_full = ref false;
  }

let spec =
  [
    ("--bin", Arg.Set_string opts.bin, " The Myte binary to test");
    ("--filter", Arg.Set_string opts.filter, " Run only tests which match a regex");
    ("--record", Arg.Set opts.record, " Re-record snapshot tests");
    ("--tree", Arg.Set opts.tree, " Include tree of all test failures in output.");
    ( "--tree-full",
      Arg.Set opts.tree_full,
      " Include tree of all test passes and failures in output." );
  ]
  |> Arg.align

let () =
  Arg.parse spec (fun _ -> ()) "Run Myte test suite";
  let bin = !(opts.bin) in
  let filter = !(opts.filter) in
  let filter =
    if filter = "" then
      None
    else
      Some (Str.regexp filter)
  in
  let record = !(opts.record) in
  let tree_full = !(opts.tree_full) in
  let tree = tree_full || !(opts.tree) in

  let suite_results =
    Runner.run_suites
      filter
      [
        Parser_tests.suite ~bin ~record;
        Analyze_tests.suite ~bin ~record;
        Mir_tests.suite ~bin ~record;
      ]
  in
  let collated_results = Collate.collate_results suite_results in
  let formatted_results = Collate.pp ~tree ~tree_full collated_results in
  Printf.printf "%s" formatted_results;

  if Collate.has_failed collated_results then
    exit 1
  else
    exit 0
