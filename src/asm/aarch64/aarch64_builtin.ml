open Aarch64_calling_convention
open Asm
open Asm_builders
open Mir_builtin
open Mir_type

(* Assmebly implemented functions *)

let myte_copy_label = "__myte_copy"

(* Syscall wrapper functions *)

let myte_close_label = "__myte_close"

let myte_exit_label = "__myte_exit"

let myte_open_label = "__myte_open"

let myte_read_label = "__myte_read"

let myte_unlink_label = "__myte_unlink"

let myte_write_label = "__myte_write"

(* Myte C runtime functions. Reference the C runtime function directly. *)

let myte_alloc_label = "mytec_alloc"

let myte_collect_label = "mytec_collect"

let myte_get_heap_size_label = "mytec_get_heap_size"

(* Create a default function signature that matches the MirBuiltin using the C calling convention *)
let mk_default_asm_builtin_function (mir_builtin : Builtin.t) (label : label) =
  let calling_convention = aapcs64 in
  let param_types = calling_convention#calculate_param_types mir_builtin.param_types in
  let func =
    mk_function ~label ~param_types ~return_type:mir_builtin.return_type ~calling_convention
  in
  (mir_builtin, func)

(* Create a function signature that matches the MirBuiltin using the C calling convention but
   prepends an int argument. Useful for openat and unlinkat syscalls. *)
let mk_prepend_int_asm_builtin_function (mir_builtin : Builtin.t) (label : label) =
  let calling_convention = aapcs64 in
  (* Implicit `int fd` first argument which is passed as part of the assembly wrapper, so only
     pass later arguments. *)
  let param_types =
    calling_convention#calculate_param_types (Type.Int :: mir_builtin.param_types)
  in
  let param_types = Array.sub param_types 1 (Array.length param_types - 1) in
  let func =
    mk_function ~label ~param_types ~return_type:mir_builtin.return_type ~calling_convention
  in
  (mir_builtin, func)

let asm_builtins : Function.t MirBuiltinMap.t ref = ref MirBuiltinMap.empty

let init () =
  asm_builtins :=
    [
      mk_default_asm_builtin_function myte_copy myte_copy_label;
      mk_default_asm_builtin_function myte_exit myte_exit_label;
      mk_default_asm_builtin_function myte_write myte_write_label;
      mk_default_asm_builtin_function myte_read myte_read_label;
      mk_default_asm_builtin_function myte_close myte_close_label;
      mk_prepend_int_asm_builtin_function myte_open myte_open_label;
      mk_prepend_int_asm_builtin_function myte_unlink myte_unlink_label;
      mk_default_asm_builtin_function myte_alloc myte_alloc_label;
      mk_default_asm_builtin_function myte_collect myte_collect_label;
      mk_default_asm_builtin_function myte_get_heap_size myte_get_heap_size_label;
    ]
    |> List.to_seq
    |> MirBuiltinMap.of_seq

let get_asm_builtin (mir_builtin : Builtin.t) : Function.t =
  match MirBuiltinMap.find_opt mir_builtin !asm_builtins with
  | Some func -> func
  | None ->
    failwith (Printf.sprintf "No builtin aarch64 function for Mir builtin %s" mir_builtin.name)
