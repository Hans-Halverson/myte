(* open Mir *)

(* let can_inline_get_pointer_instr ~gcx get_ptr_instr = () *)

(* let compile_get_pointer ~gcx { GetPointer.var_id = _; return_ty; pointer; pointer_offset; offsets }
    =
    let element_ty = pointer_value_element_type pointer in

    let offset = ref 0 in

    (match pointer_offset with
    | Some ((`ByteL _ | `IntL _ | `LongL _) as count_lit) -> ()
    | Some ((`ByteV _ | `IntV _ | `LongV _) as count_var) -> ()
    | None -> ());
  () *)
