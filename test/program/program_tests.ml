let mk_command ~optimize bin files =
  let optimize_flag =
    if optimize then
      " -O"
    else
      ""
  in
  Printf.sprintf
    {|
      %s%s %s -o t.out;
      ./t.out;
      EXIT_CODE="$?";
      if [ "$EXIT_CODE" -ne 0 ]; then
        if [ "$EXIT_CODE" -eq 139 ]; then
          EXTRA_INFO=" (Segfault)"
        fi
        echo "NOTE: Exited with code $EXIT_CODE$EXTRA_INFO"
      fi
      rm t.out 2> /dev/null;
    |}
    bin
    optimize_flag
    files

let commands ~config:_ bin files =
  let files = String.concat " " files in
  [
    ("UNOPTIMIZED", mk_command ~optimize:false bin files);
    ("OPTIMIZED", mk_command ~optimize:true bin files);
  ]

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (commands bin)