open Basic_collections
open Mir_type

module Builtin = struct
  type t = {
    name: string;
    (* Params and return type are dynamic. *)
    param_types: Type.t list;
    return_type: Type.t option;
  }

  let compare b1 b2 = String.compare b1.name b2.name
end

module MirBuiltinCollection = MakeCollection (Builtin)
module MirBuiltinMap = MirBuiltinCollection.Map

(*
    myte.builtin.alloc<T>(count: int): *T

    Allocate space for `count` adjacent values of type `T`
*)
let myte_alloc =
  { Builtin.name = "myte.builtin.alloc"; param_types = [Int]; return_type = Some (Pointer Byte) }

(*
    myte.builtin.copy<T>(dest: *T, src: *T, count: int)

    Copy `count` adjacent values of type `T` from `dest` to `src.
*)
let myte_copy =
  {
    Builtin.name = "myte.builtin.copy";
    param_types = [Pointer Byte; Pointer Byte; Int];
    return_type = None;
  }

(*
    myte.builtin.exit(exitCode: int)

    Exit with return code `exitCode`.
*)
let myte_exit = { Builtin.name = "myte.builtin.exit"; param_types = [Int]; return_type = None }

(*
    myte.builtin.write(file: int, buffer: byte*, size: int): int

    Write `size` bytes of `buffer` to file with descriptor `file`.
*)
let myte_write =
  {
    Builtin.name = "myte.builtin.write";
    param_types = [Int; Pointer Byte; Int];
    return_type = Some Int;
  }

(*
    myte.builtin.read(file: int, buffer: byte*, size: int): int

    Read `size` bytes into `buffer` from file with descriptor `file`.
*)
let myte_read =
  {
    Builtin.name = "myte.builtin.read";
    param_types = [Int; Pointer Byte; Int];
    return_type = Some Int;
  }

(*
    myte.builtin.open(file: byte*, flags: int, mode: int): int

    Open file with name `file` using `flags` and `mode` options.
*)
let myte_open =
  {
    Builtin.name = "myte.builtin.open";
    param_types = [Pointer Byte; Int; Int];
    return_type = Some Int;
  }

(*
    myte.builtin.close(file: int): int

    Close file with descriptor `file`.
*)
let myte_close =
  { Builtin.name = "myte.builtin.close"; param_types = [Int]; return_type = Some Int }

(*
    myte.builtin.unlink(file: int): int

    Unlink file with name `file`.
*)
let myte_unlink =
  { Builtin.name = "myte.builtin.unlink"; param_types = [Int]; return_type = Some Int }

(*
    myte.builtin.collect()

    Runs a garbage collection cycle.
*)
let myte_collect = { Builtin.name = "myte.builtin.collect"; param_types = []; return_type = None }

(*
    myte.builtin.get_heap_size(): long

    Return size of used myte heap in bytes.
*)
let myte_get_heap_size =
  { Builtin.name = "myte.builtin.get_heap_size"; param_types = []; return_type = Some Long }
