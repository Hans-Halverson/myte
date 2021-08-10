let commands ~config:_ bin files =
  let files = String.concat " " files in
  [
    ( "test",
      Printf.sprintf
        {|
          %s --dump-asm --no-pretty-print %s && %s %s -o t.out;
          ./t.out;
          EXIT_CODE="$?";
          if [[ "$EXIT_CODE" -ne 0 ]]; then
            if [[ "$EXIT_CODE" -eq 139 ]]; then
              EXTRA_INFO=" (Segfault)"
            fi
            echo "NOTE: Exited with code $EXIT_CODE$EXTRA_INFO"
          fi
          rm t.out 2> /dev/null
        |}
        bin
        files
        bin
        files );
  ]

let snapshot_suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (commands bin)

let suite ~bin ~record =
  let snapshot_suite = snapshot_suite ~bin ~record in
  { snapshot_suite with suites = Liveness_analysis.suite :: snapshot_suite.suites; tests = [] }
