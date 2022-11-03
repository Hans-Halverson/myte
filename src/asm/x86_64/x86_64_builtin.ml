open Asm
open Asm_builders
open Mir_builtin
open X86_64_calling_convention

(* Assembly implemented functions *)

let myte_copy_label = "__myte_copy"

(* Syscall/libc wrapper functions *)

let myte_close_label = "__myte_close"

let myte_exit_label = "__myte_exit"

let myte_open_label = "__myte_open"

let myte_read_label = "__myte_read"

let myte_unlink_label = "__myte_unlink"

let myte_write_label = "__myte_write"

(* Myte C runtime functions *)

let myte_alloc_label = "__myte_alloc"

let myte_collect_label = "__myte_collect"

let myte_get_heap_size_label = "__myte_get_heap_size"

let mk_asm_builtin_function ((mir_builtin, label) : Builtin.t * label) =
  (* All builtins use the platform's C calling convention *)
  let calling_convention = system_v_calling_convention in
  let param_types = calling_convention#calculate_param_types mir_builtin.param_types in

  let func =
    mk_function ~label ~param_types ~return_type:mir_builtin.return_type ~calling_convention
  in
  (mir_builtin, func)

let asm_builtins : Function.t MirBuiltinMap.t ref = ref MirBuiltinMap.empty

(* Initialize with the platforms C calling convention, which all builtins use *)
let init () =
  asm_builtins :=
    [
      (myte_copy, myte_copy_label);
      (myte_exit, myte_exit_label);
      (myte_write, myte_write_label);
      (myte_read, myte_read_label);
      (myte_open, myte_open_label);
      (myte_close, myte_close_label);
      (myte_unlink, myte_unlink_label);
      (myte_alloc, myte_alloc_label);
      (myte_collect, myte_collect_label);
      (myte_get_heap_size, myte_get_heap_size_label);
    ]
    |> List.map mk_asm_builtin_function
    |> List.to_seq
    |> MirBuiltinMap.of_seq

let get_asm_builtin (mir_builtin : Builtin.t) : Function.t =
  match MirBuiltinMap.find_opt mir_builtin !asm_builtins with
  | Some func -> func
  | None ->
    failwith (Printf.sprintf "No builtin x86_64 function for Mir builtin %s" mir_builtin.name)
