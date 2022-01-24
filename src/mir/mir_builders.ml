open Mir
open Mir_type

(* Literals *)

let mk_bool_lit (b : bool) : Value.t = Lit (Bool b)

let mk_byte_lit (n : int) : Value.t = Lit (Byte n)

let mk_int_lit (n : int) : Value.t = Lit (Int (Int32.of_int n))

let mk_int_lit_of_int32 (n : Int32.t) : Value.t = Lit (Int n)

let mk_long_lit (n : Int64.t) : Value.t = Lit (Long n)

let mk_global_lit (global : Global.t) : Value.t = Lit (Global global)

let mk_null_ptr_lit (type_ : Type.t) : Value.t = Lit (NullPointer type_)

let mk_func_lit (func : Function.t) : Value.t = Lit (Function func)

let mk_myte_builtin_lit (func : string) : Value.t = Lit (MyteBuiltin func)

let mk_array_string_lit (string : string) : Value.t = Lit (ArrayString string)

let mk_array_vtable_lit (funcs : Function.t list) : Value.t =
  let size = List.length funcs in
  Lit (ArrayVtable (size, funcs))

(* Instructions *)

let mk_instr ~(type_ : Type.t) ~(instr : Instruction.instr) : Instruction.t =
  let rec instruction =
    {
      Instruction.id = mk_value_id ();
      type_;
      instr;
      prev = instruction;
      next = instruction;
      block = null_block;
    }
  in
  instruction

let mk_phi ~(type_ : Type.t) ~(args : Value.t BlockMap.t) : Instruction.t =
  mk_instr ~type_ ~instr:(Phi { args })

let mk_mov ~(arg : Value.t) : Instruction.t = mk_instr ~type_:(type_of_value arg) ~instr:(Mov arg)

let mk_stack_alloc ~(type_ : Type.t) : Instruction.t =
  mk_instr ~type_:(Pointer type_) ~instr:(StackAlloc type_)

let mk_load ~(ptr : Value.t) : Instruction.t =
  match type_of_value ptr with
  | Pointer type_ -> mk_instr ~type_ ~instr:(Load ptr)
  | _ -> failwith "Load argument must be a pointer type"

let mk_store ~(ptr : Value.t) ~(value : Value.t) : Instruction.t =
  if not (types_equal (pointer_value_element_type ptr) (type_of_value value)) then
    failwith "Stored pointer and value types do not match";
  mk_instr ~type_:no_return_type ~instr:(Store (ptr, value))

let mk_get_pointer_instr
    ?(pointer_offset = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.offset list)
    () =
  if not (is_pointer_value ptr) then failwith "GetPointer argument must be a pointer type";
  mk_instr ~type_:(Pointer type_) ~instr:(GetPointer { pointer = ptr; pointer_offset; offsets })

let mk_call ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) =
  if not (is_function_value func) then failwith "Call function argument must have function type";
  let (type_, has_return) =
    match return with
    | Some type_ -> (type_, true)
    | None -> (no_return_type, false)
  in
  mk_instr ~type_ ~instr:(Call { func; args; has_return })

let mk_ret ~(arg : Value.t option) = mk_instr ~type_:no_return_type ~instr:(Ret arg)

let mk_unary ~(op : Instruction.unary_operation) ~(arg : Value.t) : Instruction.t =
  if not (is_numeric_value arg) then failwith "Unary argument must be numeric value";
  mk_instr ~type_:(type_of_value arg) ~instr:(Unary (op, arg))

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
  mk_instr ~type_:(type_of_value left) ~instr:(Binary (op, left, right))

let mk_cmp ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) : Instruction.t =
  if not (is_comparable_value left && is_comparable_value right && values_have_same_type left right)
  then
    failwith "Cmp arguments must be numeric or pointers and have the same type";
  mk_instr ~type_:Bool ~instr:(Cmp (cmp, left, right))

let mk_cast ~(arg : Value.t) ~(type_ : Type.t) : Instruction.t =
  if not (is_pointer_value arg && is_pointer_type type_) then
    failwith "Cast arguments must be pointers";
  mk_instr ~type_ ~instr:(Cast arg)

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
  mk_instr ~type_ ~instr:(Trunc arg)

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
  mk_instr ~type_ ~instr:(SExt arg)
