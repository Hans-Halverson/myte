let find_runtime_file runtime_basename =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Filename.concat absolute_dir runtime_basename

let macos_runtime_file = find_runtime_file "macos_runtime.S"

let linux_runtime_file = find_runtime_file "linux_runtime.S"

let myte_runtime_init_label = "__myte_runtime_init"

let myte_alloc_label = "__myte_alloc"

let myte_close_label = "__myte_close"

let myte_copy_label = "__myte_copy"

let myte_exit_label = "__myte_exit"

let myte_get_heap_size = "__myte_get_heap_size"

let myte_open_label = "__myte_open"

let myte_read_label = "__myte_read"

let myte_unlink_label = "__myte_unlink"

let myte_write_label = "__myte_write"
