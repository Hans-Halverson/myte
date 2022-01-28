open Mir
open Mir_type

(* Values *)

let uninit_value : Value.value = Value.Lit (Bool true)

let mk_value (value : Value.value) : Value.t = { value; uses = None }

let mk_uninit_value () : Value.t = { value = uninit_value; uses = None }

(* Literals *)

let mk_bool_lit (b : bool) : Value.t = mk_value (Lit (Bool b))

let mk_byte_lit (n : int) : Value.t = mk_value (Lit (Byte n))

let mk_int_lit (n : int) : Value.t = mk_value (Lit (Int (Int32.of_int n)))

let mk_int_lit_of_int32 (n : Int32.t) : Value.t = mk_value (Lit (Int n))

let mk_long_lit (n : Int64.t) : Value.t = mk_value (Lit (Long n))

let mk_null_ptr_lit (type_ : Type.t) : Value.t = mk_value (Lit (NullPointer type_))

let mk_myte_builtin_lit (func : string) : Value.t = mk_value (Lit (MyteBuiltin func))

let mk_array_string_lit (string : string) : Value.t = mk_value (Lit (ArrayString string))

let mk_array_vtable_lit (funcs : Function.t list) : Value.t =
  let size = List.length funcs in
  mk_value (Lit (ArrayVtable (size, funcs)))

(* Uses *)

let add_use_link (u1 : Use.t) (u2 : Use.t) =
  u1.next <- u2;
  u2.prev <- u1

let user_add_use ~(user : Value.t) ~(use : Value.t) =
  match use.uses with
  | None ->
    let rec use_node = { Use.value = use; prev = use_node; next = use_node; user } in
    use.uses <- Some use_node;
    use_node
  | Some first_use ->
    let rec use_node = { Use.value = use; prev = use_node; next = use_node; user } in
    add_use_link use_node first_use.next;
    add_use_link first_use use_node;
    use_node

let remove_use ~(use : Use.t) =
  let value = use.value in
  if use.next == use then
    value.uses <- None
  else
    let prev = use.prev in
    let next = use.next in
    add_use_link prev next;
    if Option.get value.uses == use then value.uses <- Some use.next

(* Globals *)

let mk_global
    ~(name : label)
    ~(loc : Loc.t)
    ~(type_ : Type.t)
    ~(init_val : Value.t option)
    ~(is_constant : bool) : Global.t =
  let value = mk_uninit_value () in
  let init_val_use = Option.map (fun init_val -> user_add_use ~user:value ~use:init_val) init_val in
  let global = { Global.loc; name; type_; init_val = init_val_use; is_constant; value } in
  value.value <- Lit (Global global);
  global

let global_set_init ~(global : Global.t) ~(init : Value.t option) =
  (* TODO: Check if the old and new value are the same *)
  (match global.init_val with
  | None -> ()
  | Some old_init_use -> remove_use ~use:old_init_use);
  let init_use = Option.map (fun init -> user_add_use ~user:global.value ~use:init) init in
  global.init_val <- init_use

(* Functions *)

let mk_function ~(name : label) : Function.t =
  let value = mk_uninit_value () in
  let func =
    {
      Function.name;
      loc = Loc.none;
      params = [];
      return_type = None;
      start_block = null_block;
      blocks = BlockSet.empty;
      value;
    }
  in
  value.value <- Lit (Function func);
  func

let mk_argument ~(func : Function.t) ~(decl_loc : Loc.t) ~(type_ : Type.t) : Value.t =
  let argument = { Argument.id = mk_value_id (); type_; func; decl_loc } in
  mk_value (Argument argument)

(* Instructions *)

(* Creates an instruction that is not yet part of a block *)
let mk_blockless_instr ~(type_ : Type.t) ~(instr : Instruction.instr) : Instruction.t =
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

(* Create an instruction and appends it the end of a block *)
let mk_instr ~(block : Block.t) ~(type_ : Type.t) ~(instr : Instruction.instr) : Instruction.t =
  let instruction = mk_blockless_instr ~type_ ~instr in
  append_instruction block instruction;
  instruction

let mk_blockless_phi ~(type_ : Type.t) ~(args : Value.t BlockMap.t) : Value.t =
  let value = mk_uninit_value () in
  let args = BlockMap.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let instr = mk_blockless_instr ~type_ ~instr:(Phi { args }) in
  value.value <- Instr instr;
  value

let mk_blockless_mov ~(arg : Value.t) : Value.t =
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_blockless_instr ~type_:(type_of_value arg) ~instr:(Mov arg_use) in
  value.value <- Instr instr;
  value

let mk_blockless_stack_alloc ~(type_ : Type.t) : Value.t =
  let instr = mk_blockless_instr ~type_:(Pointer type_) ~instr:(StackAlloc type_) in
  mk_value (Instr instr)

let mk_stack_alloc ~(block : Block.t) ~(type_ : Type.t) : Value.t =
  let instr = mk_instr ~block ~type_:(Pointer type_) ~instr:(StackAlloc type_) in
  mk_value (Instr instr)

let mk_load ~(block : Block.t) ~(ptr : Value.t) : Value.t =
  match type_of_value ptr with
  | Pointer type_ ->
    let value = mk_uninit_value () in
    let ptr_use = user_add_use ~user:value ~use:ptr in
    let instr = mk_instr ~block ~type_ ~instr:(Load ptr_use) in
    value.value <- Instr instr;
    value
  | _ -> failwith "Load argument must be a pointer type"

let mk_store_ ~(block : Block.t) ~(ptr : Value.t) ~(value : Value.t) : unit =
  if not (types_equal (pointer_value_element_type ptr) (type_of_value value)) then
    failwith "Stored pointer and value types do not match";
  let instr_value = mk_uninit_value () in
  let ptr_use = user_add_use ~user:instr_value ~use:ptr in
  let value_use = user_add_use ~user:instr_value ~use:value in
  let instr = mk_instr ~block ~type_:no_return_type ~instr:(Store (ptr_use, value_use)) in
  instr_value.value <- Instr instr

let mk_get_pointer_instr
    ~(block : Block.t)
    ?(pointer_offset : Value.t option = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.value_offset list)
    () : Value.t =
  if not (is_pointer_value ptr) then failwith "GetPointer argument must be a pointer type";
  let value = mk_uninit_value () in
  let ptr_use = user_add_use ~user:value ~use:ptr in
  let pointer_offset_use =
    Option.map (fun offset -> user_add_use ~user:value ~use:offset) pointer_offset
  in
  let use_offsets =
    List.map
      (fun offset ->
        match offset with
        | Instruction.GetPointer.PointerIndex arg ->
          Instruction.GetPointer.PointerIndex (user_add_use ~user:value ~use:arg)
        | FieldIndex index -> FieldIndex index)
      offsets
  in
  let instr =
    mk_instr
      ~block
      ~type_:(Pointer type_)
      ~instr:
        (GetPointer
           { pointer = ptr_use; pointer_offset = pointer_offset_use; offsets = use_offsets })
  in
  value.value <- Instr instr;
  value

let mk_call ~(block : Block.t) ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) :
    Value.t =
  if not (is_function_value func) then failwith "Call function argument must have function type";
  let value = mk_uninit_value () in
  let func_use = user_add_use ~user:value ~use:func in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let (type_, has_return) =
    match return with
    | Some type_ -> (type_, true)
    | None -> (no_return_type, false)
  in
  let instr =
    mk_instr ~block ~type_ ~instr:(Call { func = Value func_use; args = arg_uses; has_return })
  in
  value.value <- Instr instr;
  value

let mk_call_ ~block ~func ~args ~return : unit = ignore (mk_call ~block ~func ~args ~return)

let mk_call_builtin
    ~(block : Block.t) (builtin : Builtin.t) (args : Value.t list) (mk_return_ty_args : Type.t list)
    : Value.t =
  let value = mk_uninit_value () in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let return_type = builtin.mk_return_ty mk_return_ty_args |> Option.get in
  let instr =
    mk_instr
      ~block
      ~type_:return_type
      ~instr:(Call { func = MirBuiltin builtin; args = arg_uses; has_return = true })
  in
  value.value <- Instr instr;
  value

let mk_call_builtin_no_return_ ~(block : Block.t) (builtin : Builtin.t) (args : Value.t list) =
  let value = mk_uninit_value () in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let instr =
    mk_instr
      ~block
      ~type_:no_return_type
      ~instr:(Call { func = MirBuiltin builtin; args = arg_uses; has_return = false })
  in
  value.value <- Instr instr

let mk_ret_ ~(block : Block.t) ~(arg : Value.t option) =
  let value = mk_uninit_value () in
  let arg_use = Option.map (fun arg -> user_add_use ~user:value ~use:arg) arg in
  let instr = mk_instr ~block ~type_:no_return_type ~instr:(Ret arg_use) in
  value.value <- Instr instr

let mk_unary ~(block : Block.t) ~(op : Instruction.unary_operation) ~(arg : Value.t) : Value.t =
  if not (is_numeric_value arg) then failwith "Unary argument must be numeric value";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_:(type_of_value arg) ~instr:(Unary (op, arg_use)) in
  value.value <- Instr instr;
  value

let mk_binary
    ~(block : Block.t) ~(op : Instruction.binary_operation) ~(left : Value.t) ~(right : Value.t) :
    Value.t =
  let is_shift_op = is_shift_op op in
  if is_shift_op && not (is_numeric_value left && is_numeric_value right) then
    failwith "Shift arguments must be numeric"
  else if
    (not is_shift_op)
    && not (is_numeric_value left && is_numeric_value right && values_have_same_type left right)
  then
    failwith "Binary arguments must be numeric and have the same type";
  let value = mk_uninit_value () in
  let left_use = user_add_use ~user:value ~use:left in
  let right_use = user_add_use ~user:value ~use:right in
  let instr =
    mk_instr ~block ~type_:(type_of_value left) ~instr:(Binary (op, left_use, right_use))
  in
  value.value <- Instr instr;
  value

let mk_cmp ~(block : Block.t) ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) :
    Value.t =
  if not (is_comparable_value left && is_comparable_value right && values_have_same_type left right)
  then
    failwith "Cmp arguments must be numeric or pointers and have the same type";
  let value = mk_uninit_value () in
  let left_use = user_add_use ~user:value ~use:left in
  let right_use = user_add_use ~user:value ~use:right in
  let instr = mk_instr ~block ~type_:Bool ~instr:(Cmp (cmp, left_use, right_use)) in
  value.value <- Instr instr;
  value

let mk_cast ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  if not (is_pointer_value arg && is_pointer_type type_) then
    failwith "Cast arguments must be pointers";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_ ~instr:(Cast arg_use) in
  value.value <- Instr instr;
  value

let mk_trunc ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  let arg_type = type_of_value arg in
  if
    not
      ( is_numeric_type arg_type
      && is_numeric_type type_
      && size_of_type arg_type >= size_of_type type_ )
  then
    failwith
      "Trunc arguments must be numeric with type argument having smaller size than value argument";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_ ~instr:(Trunc arg_use) in
  value.value <- Instr instr;
  value

let mk_sext ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  let arg_type = type_of_value arg in
  if
    not
      ( is_numeric_type arg_type
      && is_numeric_type type_
      && size_of_type arg_type <= size_of_type type_ )
  then
    failwith
      "SExt arguments must be numeric with type argument having larger size than value argument";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_ ~instr:(SExt arg_use) in
  value.value <- Instr instr;
  value

let mk_unreachable_ ~(block : Block.t) : unit =
  ignore (mk_instr ~block ~type_:no_return_type ~instr:Unreachable)

let mk_continue_ ~(block : Block.t) ~(continue : Block.t) : unit =
  ignore (mk_instr ~block ~type_:no_return_type ~instr:(Continue continue))

let mk_branch_ ~(block : Block.t) ~(test : Value.t) ~(continue : Block.t) ~(jump : Block.t) : unit =
  let value = mk_uninit_value () in
  let test_use = user_add_use ~user:value ~use:test in
  let instr =
    mk_instr ~block ~type_:no_return_type ~instr:(Branch { test = test_use; continue; jump })
  in
  value.value <- Instr instr
