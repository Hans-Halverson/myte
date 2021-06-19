open Mir

(* 
   myte.myte_alloc<T>(count: long): *T

   Allocate space for `count` adjacent values of type `T`
 *)
let myte_alloc = { Builtin.name = "myte_alloc"; mk_return_ty = (fun ty -> `PointerT ty) }

let mk_call_builtin builtin var_id args mk_return_ty_args =
  let open Builtin in
  let return_ty = builtin.mk_return_ty mk_return_ty_args in
  let result_val = var_value_of_type var_id return_ty in
  (result_val, Instruction.CallBuiltin (var_id, return_ty, builtin, args))
