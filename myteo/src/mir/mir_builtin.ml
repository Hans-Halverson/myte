open Mir

(* 
   myte.myte_alloc<T>(count: long): *T

   Allocate space for `count` adjacent values of type `T`
 *)
let myte_alloc =
  { Builtin.name = "myte_alloc"; mk_return_ty = (fun tys -> `PointerT (List.hd tys)) }

(* 
   myte.myte_alloc<T>(file: int, buffer: byte*, size: int): int

   Write `size` bytes of `buffer` to file with descriptor `file`.
 *)
let myte_write = { Builtin.name = "myte_write"; mk_return_ty = (fun _ -> `IntT) }

let mk_call_builtin builtin var_id args mk_return_ty_args =
  let open Builtin in
  let return_ty = builtin.mk_return_ty mk_return_ty_args in
  let result_val = var_value_of_type var_id return_ty in
  (result_val, Instruction.CallBuiltin (var_id, return_ty, builtin, args))
