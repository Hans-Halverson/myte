let myte_bin_regex = Str.regexp "<BIN>"

let files_regex = Str.regexp "<MYTE_FILES>"

let commands ~config bin files =
  [
    ( "test",
      Option.value ~default:"" config
      |> Str.global_replace myte_bin_regex bin
      |> Str.global_replace files_regex (String.concat " " files) );
  ]

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (commands bin)
