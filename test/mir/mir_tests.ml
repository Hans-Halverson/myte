let commands ~config bin files =
  let config =
    match config with
    | None -> ""
    | Some config -> config ^ " "
  in
  [
    ( "test",
      Printf.sprintf "%s --no-pretty-print --dump-ir %s%s" bin config (String.concat " " files) );
  ]

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (commands bin)
