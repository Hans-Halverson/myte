let command ~config bin files =
  Printf.sprintf
    "%s --no-pretty-print %s %s"
    bin
    (Option.value ~default:"" config)
    (String.concat " " files)

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (command bin)
