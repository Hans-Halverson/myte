let macos_runtime_file () = Filename.concat (Installation.get_runtime_path ()) "macos.o"

let linux_runtime_file () = Filename.concat (Installation.get_runtime_path ()) "linux.o"

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
