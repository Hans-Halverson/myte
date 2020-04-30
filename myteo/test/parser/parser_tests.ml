let command bin files = Printf.sprintf "%s --show-ast --no-pretty-print %s" bin (String.concat " " files)

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (command bin)