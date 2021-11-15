(* Write the given runtime file to disk and return its name *)
let mk_runtime_file name contents =
  let file = open_out name in
  output_string file contents;
  close_out file;
  name

let macos_runtime_file () = mk_runtime_file "macos_runtime.S" Macos_runtime.file

let linux_runtime_file () = mk_runtime_file "linux_runtime.S" Linux_runtime.file

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
