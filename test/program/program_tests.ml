let mk_command ~config ~optimize ~target ~run_prefix bin files =
  let args =
    match config with
    | None -> ""
    | Some config -> " " ^ config
  in
  let optimize_flag =
    if optimize then
      " -O"
    else
      ""
  in
  let target_triple = Target.target_triple target in
  Printf.sprintf
    {|
      %s%s --target %s -o t.out %s && %s./t.out%s;
      EXIT_CODE="$?";
      rm t.out 2> /dev/null;
      if [ "$EXIT_CODE" -ne 0 ]; then
        if [ "$EXIT_CODE" -eq 139 ]; then
          EXTRA_INFO=" (Segfault)"
        fi
        echo "NOTE: Exited with code $EXIT_CODE$EXTRA_INFO"
      fi
    |}
    bin
    optimize_flag
    target_triple
    files
    run_prefix
    args

let commands ~config ~target ~run_prefix bin files =
  let files = String.concat " " files in
  [
    ("UNOPTIMIZED", mk_command ~config ~optimize:false ~target ~run_prefix bin files);
    ("OPTIMIZED", mk_command ~config ~optimize:true ~target ~run_prefix bin files);
  ]

let suite ~bin ~record ~cross =
  let host = !Target.host in
  let (target, run_prefix) =
    if cross == None || Target.equal host (Option.get cross) then
      (host, "")
    else
      let cross_target = Option.get cross in
      (cross_target, Cross.get_cross_run_prefix ~host ~cross_target)
  in

  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (commands ~target ~run_prefix bin)
