open Asm
open Asm_builders
open Basic_collections
open X86_64_calling_convention
open X86_64_runtime
open Mir_builtin

module MirBuiltinCollection = MakeCollection (Builtin)
module MirBuiltinMap = MirBuiltinCollection.Map

let mk_asm_builtin_function ((mir_builtin, label) : Builtin.t * label) =
  (* All builtins use the platform's C calling convention *)
  let calling_convention = system_v_calling_convention in
  let param_types = calling_convention#calculate_param_types mir_builtin.param_types in

  let func =
    mk_function ~label ~param_types ~return_type:mir_builtin.return_type ~calling_convention
  in
  (mir_builtin, func)

let asm_builtins : Function.t MirBuiltinMap.t =
  [
    (myte_alloc, myte_alloc_label);
    (myte_copy, myte_copy_label);
    (myte_exit, myte_exit_label);
    (myte_write, myte_write_label);
    (myte_read, myte_read_label);
    (myte_open, myte_open_label);
    (myte_close, myte_close_label);
    (myte_unlink, myte_unlink_label);
    (myte_collect, myte_collect_label);
    (myte_get_heap_size, myte_get_heap_size_label);
  ]
  |> List.map mk_asm_builtin_function
  |> List.to_seq
  |> MirBuiltinMap.of_seq

let get_asm_builtin (mir_builtin : Builtin.t) : Function.t =
  match MirBuiltinMap.find_opt mir_builtin asm_builtins with
  | Some func -> func
  | None ->
    failwith (Printf.sprintf "No x86_64 builtin function for Mir builtin %s" mir_builtin.name)
