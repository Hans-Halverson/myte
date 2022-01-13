open Mir
open Mir_type

(* Literals *)

let mk_bool_lit (b : bool) : Value.t = Lit (Bool b)

let mk_byte_lit (n : int) : Value.t = Lit (Byte n)

let mk_int_lit (n : int) : Value.t = Lit (Int (Int32.of_int n))

let mk_int_lit_of_int32 (n : Int32.t) : Value.t = Lit (Int n)

let mk_long_lit (n : Int64.t) : Value.t = Lit (Long n)

let mk_ptr_lit (type_ : Type.t) (label : label) : Value.t = Lit (Pointer (type_, label))

let mk_null_ptr_lit (type_ : Type.t) : Value.t = Lit (NullPointer type_)

let mk_func_lit (label : label) : Value.t = Lit (Function label)

let mk_array_string_lit (string : string) : Value.t = Lit (ArrayString string)

let mk_array_vtable_lit (func_labels : label list) : Value.t =
  let size = List.length func_labels in
  Lit (ArrayVtable (size, func_labels))

(* Instructions *)

let mk_stack_alloc ~(type_ : Type.t) : Instruction.t =
  { Instruction.id = mk_value_id (); type_ = `PointerT type_; instr = StackAlloc type_ }

let mk_load ~(ptr : Value.t) : Instruction.t =
  match type_of_value ptr with
  | `PointerT type_ -> { Instruction.id = mk_value_id (); type_; instr = Load ptr }
  | _ -> failwith "Load argument must be pointer"

let mk_store ~(ptr : Value.t) ~(value : Value.t) : Instruction.t =
  { Instruction.id = mk_value_id (); type_ = no_return_type; instr = Store (ptr, value) }

let mk_get_pointer_instr
    ?(pointer_offset = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.offset list)
    () =
  let instr = Instruction.GetPointer { pointer = ptr; pointer_offset; offsets } in
  { Instruction.id = mk_value_id (); type_ = `PointerT type_; instr }

let mk_call ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) =
  let (type_, has_return) =
    match return with
    | Some type_ -> (type_, true)
    | None -> (no_return_type, false)
  in
  { Instruction.id = mk_value_id (); type_; instr = Call { func = Value func; args; has_return } }

let mk_ret ~(arg : Value.t option) =
  { Instruction.id = mk_value_id (); type_ = no_return_type; instr = Ret arg }

let mk_unary ~(op : Instruction.unary_operation) ~(arg : Value.t) : Instruction.t =
  let type_ = type_of_value arg in
  { Instruction.id = mk_value_id (); type_; instr = Unary (op, arg) }

let mk_binary ~(op : Instruction.binary_operation) ~(left : Value.t) ~(right : Value.t) :
    Instruction.t =
  let type_ = type_of_value left in
  { Instruction.id = mk_value_id (); type_; instr = Binary (op, left, right) }

let mk_cmp ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) : Instruction.t =
  { Instruction.id = mk_value_id (); type_ = `BoolT; instr = Cmp (cmp, left, right) }

let mk_cast ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  { Instruction.id = mk_value_id (); type_; instr = Cast arg }

let mk_trunc ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  { Instruction.id = mk_value_id (); type_; instr = Trunc arg }

let mk_sext ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  { Instruction.id = mk_value_id (); type_; instr = SExt arg }
