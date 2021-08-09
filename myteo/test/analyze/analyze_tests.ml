let commands ~config bin files =
  [
    ( "test",
      Printf.sprintf
        "%s --no-pretty-print %s %s"
        bin
        (Option.value ~default:"--check" config)
        (String.concat " " files) );
  ]

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (commands bin)
