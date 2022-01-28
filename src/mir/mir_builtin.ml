open Mir

(* 
   myte.builtin.alloc<T>(count: int): *T

   Allocate space for `count` adjacent values of type `T`
 *)
let myte_alloc =
  { Builtin.name = "myte.builtin.alloc"; mk_return_ty = (fun tys -> Some (Pointer (List.hd tys))) }

(* 
   myte.builtin.copy<T>(dest: *T, src: *T, count: int)

   Copy `count` adjacent values of type `T` from `dest` to `src.
 *)
let myte_copy = { Builtin.name = "myte.builtin.copy"; mk_return_ty = (fun _ -> None) }

(* 
   myte.builtin.exit(exitCode: int)

   Exit with return code `exitCode`.
 *)
let myte_exit = { Builtin.name = "myte.builtin.exit"; mk_return_ty = (fun _ -> None) }

(* 
   myte.builtin.write<T>(file: int, buffer: byte*, size: int): int

   Write `size` bytes of `buffer` to file with descriptor `file`.
 *)
let myte_write = { Builtin.name = "myte.builtin.write"; mk_return_ty = (fun _ -> Some Int) }

(* 
   myte.builtin.read<T>(file: int, buffer: byte*, size: int): int

   Read `size` bytes into `buffer` from file with descriptor `file`.
 *)
let myte_read = { Builtin.name = "myte.builtin.read"; mk_return_ty = (fun _ -> Some Int) }

(* 
   myte.builtin.open<T>(file: byte*, flags: int, mode: int): int

   Open file with name `file` using `flags` and `mode` options.
 *)
let myte_open = { Builtin.name = "myte.builtin.open"; mk_return_ty = (fun _ -> Some Int) }

(* 
   myte.builtin.close<T>(file: int): int

   Close file with descriptor `file`.
 *)
let myte_close = { Builtin.name = "myte.builtin.close"; mk_return_ty = (fun _ -> Some Int) }

(* 
   myte.builtin.unlink<T>(file: byte* ): int

   Unlink file with name `file`.
 *)
let myte_unlink = { Builtin.name = "myte.builtin.unlink"; mk_return_ty = (fun _ -> Some Int) }

(* 
   myte.builtin.get_heap_size<T>(): long

   Return size of used myte heap in bytes.
 *)
let myte_get_heap_size =
  { Builtin.name = "myte.builtin.get_heap_size"; mk_return_ty = (fun _ -> Some Long) }
