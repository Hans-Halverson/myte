let config_equals config str =
  match config with
  | Some config when config = str -> true
  | _ -> false

let command ~config bin files =
  let mode =
    if config_equals config "optimized" then
      "--dump-optimized-ir"
    else if config_equals config "both" then
      "--dump-ir --dump-optimized-ir"
    else
      "--dump-ir"
  in
  Printf.sprintf "%s --no-pretty-print %s %s" bin mode (String.concat " " files)

let suite ~bin ~record =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Snapshot_test.suite ~record absolute_dir (command bin)
