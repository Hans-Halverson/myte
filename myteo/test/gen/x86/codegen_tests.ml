let snapshots_command ~config:_ bin files =
  Printf.sprintf "%s --dump-asm --no-pretty-print %s" bin (String.concat " " files)

let snapshot_suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (snapshots_command bin)

let suite ~bin ~record =
  let snapshot_suite = snapshot_suite ~bin ~record in
  { snapshot_suite with suites = Liveness_analysis.suite :: snapshot_suite.suites; tests = [] }
