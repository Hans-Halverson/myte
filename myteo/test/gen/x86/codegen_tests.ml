let snapshots_command ~config:_ bin files =
  let files = String.concat " " files in
  Printf.sprintf
    {|
      %s --dump-asm --no-pretty-print %s && %s %s -o t.out;
      ./t.out;
      if [[ $? -eq 139 ]]; then
        echo "ERROR: Segfault when running executable!"
      fi
      rm t.out 2> /dev/null
    |}
    bin
    files
    bin
    files

let snapshot_suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (snapshots_command bin)

let suite ~bin ~record =
  let snapshot_suite = snapshot_suite ~bin ~record in
  { snapshot_suite with suites = Liveness_analysis.suite :: snapshot_suite.suites; tests = [] }
