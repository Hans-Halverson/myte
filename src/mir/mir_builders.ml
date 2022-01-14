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
  { Instruction.id = mk_value_id (); type_ = Pointer type_; instr = StackAlloc type_ }

let mk_load ~(ptr : Value.t) : Instruction.t =
  match type_of_value ptr with
  | Pointer type_ -> { Instruction.id = mk_value_id (); type_; instr = Load ptr }
  | _ -> failwith "Load argument must be a pointer type"

let mk_store ~(ptr : Value.t) ~(value : Value.t) : Instruction.t =
  if not (types_equal (pointer_value_element_type ptr) (type_of_value value)) then
    failwith "Stored pointer and value types do not match";
  { Instruction.id = mk_value_id (); type_ = no_return_type; instr = Store (ptr, value) }

let mk_get_pointer_instr
    ?(pointer_offset = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.offset list)
    () =
  if not (is_pointer_value ptr) then failwith "GetPointer argument must be a pointer type";
  let instr = Instruction.GetPointer { pointer = ptr; pointer_offset; offsets } in
  { Instruction.id = mk_value_id (); type_ = Pointer type_; instr }

let mk_call ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) =
  if not (is_function_value func) then failwith "Call function argument must have function type";
  let (type_, has_return) =
    match return with
    | Some type_ -> (type_, true)
    | None -> (no_return_type, false)
  in
  { Instruction.id = mk_value_id (); type_; instr = Call { func = Value func; args; has_return } }

let mk_ret ~(arg : Value.t option) =
  { Instruction.id = mk_value_id (); type_ = no_return_type; instr = Ret arg }

let mk_unary ~(op : Instruction.unary_operation) ~(arg : Value.t) : Instruction.t =
  if not (is_numeric_value arg) then failwith "Unary argument must be numeric value";
  let type_ = type_of_value arg in
  { Instruction.id = mk_value_id (); type_; instr = Unary (op, arg) }

let mk_binary ~(op : Instruction.binary_operation) ~(left : Value.t) ~(right : Value.t) :
    Instruction.t =
  let is_shift_op = is_shift_op op in
  if is_shift_op && not (is_numeric_value left && is_numeric_value right) then
    failwith "Shift arguments must be numeric"
  else if
    (not is_shift_op)
    && not (is_numeric_value left && is_numeric_value right && values_have_same_type left right)
  then
    failwith "Binary arguments must be numeric and have the same type";
  let type_ = type_of_value left in
  { Instruction.id = mk_value_id (); type_; instr = Binary (op, left, right) }

let mk_cmp ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) : Instruction.t =
  if not (is_comparable_value left && is_comparable_value right && values_have_same_type left right)
  then
    failwith "Cmp arguments must be numeric or pointers and have the same type";
  { Instruction.id = mk_value_id (); type_ = Bool; instr = Cmp (cmp, left, right) }

let mk_cast ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  if not (is_pointer_value arg && is_pointer_type type_) then
    failwith "Cast arguments must be pointers";
  { Instruction.id = mk_value_id (); type_; instr = Cast arg }

let mk_trunc ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  let arg_type = type_of_value arg in
  if
    not
      ( is_numeric_type arg_type
      && is_numeric_type type_
      && size_of_type arg_type >= size_of_type type_ )
  then
    failwith
      "Trunc arguments must be numeric with type argument having smaller size than value argument";

  { Instruction.id = mk_value_id (); type_; instr = Trunc arg }

let mk_sext ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  let arg_type = type_of_value arg in
  if
    not
      ( is_numeric_type arg_type
      && is_numeric_type type_
      && size_of_type arg_type <= size_of_type type_ )
  then
    failwith
      "SExt arguments must be numeric with type argument having larger size than value argument";

  { Instruction.id = mk_value_id (); type_; instr = SExt arg }