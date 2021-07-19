let macos_runtime_basename = "macos_runtime.S"

let macos_runtime_file_path =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Filename.concat absolute_dir macos_runtime_basename

let myte_init_label = "__myte_init"

let myte_alloc_label = "__myte_alloc"

let myte_copy_label = "__myte_copy"

let myte_exit_label = "__myte_exit"

let myte_write_label = "__myte_write"
