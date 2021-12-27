let lib_myte_file () = Filename.concat (Installation.get_runtime_path ()) "libmyte.a"

let myte_runtime_init_label = "_myte_runtime_init"

let myte_alloc_label = "_myte_alloc"

let myte_close_label = "__myte_close"

let myte_copy_label = "__myte_copy"

let myte_exit_label = "__myte_exit"

let myte_get_heap_size = "_myte_get_heap_size"

let myte_open_label = "__myte_open"

let myte_read_label = "__myte_read"

let myte_unlink_label = "__myte_unlink"

let myte_write_label = "__myte_write"
