open Basic_collections
open Mir
open Mir_type

(*
 * ============================
 *           Values
 * ============================
 *)

let uninit_value : Value.value = Value.Lit (Bool true)

let mk_value (value : Value.value) : Value.t = { id = mk_value_id (); value; uses = None }

let mk_uninit_value () : Value.t = { id = mk_value_id (); value = uninit_value; uses = None }

(*
 * ============================
 *          Literals
 * ============================
 *)

let mk_bool_lit (b : bool) : Value.t = mk_value (Lit (Bool b))

let mk_byte_lit (n : Int8.t) : Value.t = mk_value (Lit (Byte n))

let mk_int_lit (n : int) : Value.t = mk_value (Lit (Int (Int32.of_int n)))

let mk_int_lit_of_int32 (n : Int32.t) : Value.t = mk_value (Lit (Int n))

let mk_long_lit (n : Int64.t) : Value.t = mk_value (Lit (Long n))

let mk_double_lit (n : Float.t) : Value.t = mk_value (Lit (Double n))

let mk_null_ptr_lit (type_ : Type.t) : Value.t = mk_value (Lit (NullPointer type_))

let mk_myte_builtin_lit (func : string) : Value.t = mk_value (Lit (MyteBuiltin func))

let mk_array_string_lit (string : string) : Value.t = mk_value (Lit (ArrayString string))

let rec mk_array_vtable_lit (funcs : Function.t list) : Value.t =
  let value = mk_uninit_value () in
  let size = List.length funcs in
  let func_uses = List.map (fun func -> user_add_use ~user:value ~use:func.Function.value) funcs in
  value.value <- Lit (ArrayVtable (size, func_uses));
  value

and mk_aggregate_closure (ty : Type.t) (func : Function.t) : Value.t =
  let value = mk_uninit_value () in
  let func_use = user_add_use ~user:value ~use:func.value in
  value.value <- Lit (AggregateClosure (ty, func_use));
  value

(*
 * ============================
 *   Instruction Constructors
 * ============================
 *)

(* Set a value to contain an instruction, without attaching to a block *)
and mk_blockless_instr f =
  let value = mk_uninit_value () in
  f ~value;
  value

(* Set a value to contain an instruction, and append to the end of a block *)
and mk_block_instr ~block f =
  let value = mk_uninit_value () in
  f ~value;
  append_instruction block value;
  value

(* Set a value to contain an instruction *)
and set_instr ~(value : Value.t) ~(type_ : Type.t) ~(instr : Instruction.instr) : unit =
  let instruction = { Instruction.type_; instr; prev = value; next = value; block = null_block } in
  value.value <- Instr instruction

(* Set a value to contain an instruction and appends it the end of a block *)
and mk_instr ~(value : Value.t) ~(block : Block.t) ~(type_ : Type.t) ~(instr : Instruction.instr) :
    Value.t =
  set_instr ~value ~type_ ~instr;
  append_instruction block value;
  value

and set_phi_instr ~(value : Value.t) ~(type_ : Type.t) ~(args : Value.t BlockMap.t) : unit =
  let args = BlockMap.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  set_instr ~value ~type_ ~instr:(Phi { args })

and mk_blockless_phi ~(type_ : Type.t) ~(args : Value.t BlockMap.t) : Value.t =
  mk_blockless_instr (set_phi_instr ~type_ ~args)

and set_mov_instr ~(value : Value.t) ~(arg : Value.t) : unit =
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_:(type_of_value arg) ~instr:(Mov arg_use)

and mk_blockless_mov ~(arg : Value.t) : Value.t = mk_blockless_instr (set_mov_instr ~arg)

and set_stack_alloc_instr ~(value : Value.t) ~(type_ : Type.t) : unit =
  set_instr ~value ~type_:(Pointer type_) ~instr:(StackAlloc type_) |> ignore

and mk_blockless_stack_alloc ~(type_ : Type.t) : Value.t =
  mk_blockless_instr (set_stack_alloc_instr ~type_)

and mk_stack_alloc ~(block : Block.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_stack_alloc_instr ~type_)

and set_load_instr ~(value : Value.t) ~(ptr : Value.t) : unit =
  match type_of_value ptr with
  | Pointer type_ ->
    let ptr_use = user_add_use ~user:value ~use:ptr in
    set_instr ~value ~type_ ~instr:(Load ptr_use)
  | _ -> failwith "Load argument must be a pointer type"

and mk_load ~(block : Block.t) ~(ptr : Value.t) : Value.t =
  mk_block_instr ~block (set_load_instr ~ptr)

and set_store_instr ~(instr_value : Value.t) ~(ptr : Value.t) ~(stored_value : Value.t) : unit =
  if not (types_equal (pointer_value_element_type ptr) (type_of_value stored_value)) then
    failwith "Stored pointer and value types do not match";
  let ptr_use = user_add_use ~user:instr_value ~use:ptr in
  let value_use = user_add_use ~user:instr_value ~use:stored_value in
  ignore (set_instr ~value:instr_value ~type_:no_return_type ~instr:(Store (ptr_use, value_use)))

and mk_store_ ~(block : Block.t) ~(ptr : Value.t) ~(value : Value.t) : unit =
  mk_block_instr ~block (fun ~value:instr_value ->
      set_store_instr ~instr_value ~ptr ~stored_value:value)
  |> ignore

and set_get_pointer_instr
    ~(value : Value.t)
    ?(pointer_offset : Value.t option = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.value_offset list)
    () : unit =
  if not (is_pointer_value ptr) then failwith "GetPointer argument must be a pointer type";
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
  set_instr
    ~value
    ~type_:(Pointer type_)
    ~instr:
      (GetPointer { pointer = ptr_use; pointer_offset = pointer_offset_use; offsets = use_offsets })

and mk_get_pointer_instr
    ~(block : Block.t)
    ?(pointer_offset : Value.t option = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.value_offset list)
    () : Value.t =
  mk_block_instr ~block (set_get_pointer_instr ~pointer_offset ~type_ ~ptr ~offsets ())

and set_call_instr
    ~(value : Value.t) ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) : unit =
  if not (is_function_value func) then failwith "Call function argument must have function type";
  let func_use = user_add_use ~user:value ~use:func in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let (type_, has_return) =
    match return with
    | Some type_ -> (type_, true)
    | None -> (no_return_type, false)
  in
  set_instr ~value ~type_ ~instr:(Call { func = Value func_use; args = arg_uses; has_return })

and mk_call ~(block : Block.t) ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) :
    Value.t =
  mk_block_instr ~block (set_call_instr ~func ~args ~return)

and mk_call_ ~block ~func ~args ~return : unit = ignore (mk_call ~block ~func ~args ~return)

and set_call_builtin_instr
    ~(value : Value.t) ~(builtin : Builtin.t) ~(args : Value.t list) ~(return : Type.t option) :
    unit =
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let (type_, has_return) =
    match return with
    | None -> (no_return_type, false)
    | Some type_ -> (type_, true)
  in
  set_instr ~value ~type_ ~instr:(Call { func = MirBuiltin builtin; args = arg_uses; has_return })

and mk_blockless_call_builtin
    (builtin : Builtin.t) (args : Value.t list) (mk_return_ty_args : Type.t list) : Value.t =
  let return = builtin.mk_return_ty mk_return_ty_args in
  mk_blockless_instr (set_call_builtin_instr ~builtin ~args ~return)

and mk_call_builtin
    ~(block : Block.t) (builtin : Builtin.t) (args : Value.t list) (mk_return_ty_args : Type.t list)
    : Value.t =
  let return = builtin.mk_return_ty mk_return_ty_args in
  mk_block_instr ~block (set_call_builtin_instr ~builtin ~args ~return)

and mk_call_builtin_no_return_ ~(block : Block.t) (builtin : Builtin.t) (args : Value.t list) =
  mk_block_instr ~block (set_call_builtin_instr ~builtin ~args ~return:None) |> ignore

and set_ret_instr ~(value : Value.t) ~(arg : Value.t option) : unit =
  let arg_use = Option.map (fun arg -> user_add_use ~user:value ~use:arg) arg in
  set_instr ~value ~type_:no_return_type ~instr:(Ret arg_use)

and mk_ret_ ~(block : Block.t) ~(arg : Value.t option) : unit =
  mk_block_instr ~block (set_ret_instr ~arg) |> ignore

and set_unary_instr ~(value : Value.t) ~(op : Instruction.unary_operation) ~(arg : Value.t) : unit =
  if not (is_numeric_value arg) then failwith "Unary argument must be numeric value";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_:(type_of_value arg) ~instr:(Unary (op, arg_use))

and mk_unary ~(block : Block.t) ~(op : Instruction.unary_operation) ~(arg : Value.t) : Value.t =
  mk_block_instr ~block (set_unary_instr ~op ~arg)

and set_binary_instr
    ~(value : Value.t) ~(op : Instruction.binary_operation) ~(left : Value.t) ~(right : Value.t) :
    unit =
  let is_shift_op = is_shift_op op in
  if is_shift_op && not (is_integer_value left && is_integer_value right) then
    failwith "Shift arguments must be integers"
  else if
    (not is_shift_op)
    && not (is_numeric_value left && is_numeric_value right && values_have_same_type left right)
  then
    failwith "Binary arguments must be numeric and have the same type";
  let left_use = user_add_use ~user:value ~use:left in
  let right_use = user_add_use ~user:value ~use:right in
  set_instr ~value ~type_:(type_of_value left) ~instr:(Binary (op, left_use, right_use))

and mk_binary
    ~(block : Block.t) ~(op : Instruction.binary_operation) ~(left : Value.t) ~(right : Value.t) :
    Value.t =
  mk_block_instr ~block (set_binary_instr ~op ~left ~right)

and set_cmp_instr
    ~(value : Value.t) ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) : unit =
  if not (is_comparable_value left && is_comparable_value right && values_have_same_type left right)
  then
    failwith "Cmp arguments must be numeric or pointers and have the same type";
  let left_use = user_add_use ~user:value ~use:left in
  let right_use = user_add_use ~user:value ~use:right in
  set_instr ~value ~type_:Bool ~instr:(Cmp (cmp, left_use, right_use))

and mk_blockless_cmp ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) : Value.t
    =
  mk_blockless_instr (set_cmp_instr ~cmp ~left ~right)

and mk_cmp ~(block : Block.t) ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) :
    Value.t =
  mk_block_instr ~block (set_cmp_instr ~cmp ~left ~right)

and set_cast_instr ~(value : Value.t) ~(arg : Value.t) ~(type_ : Type.t) : unit =
  if not (is_pointer_value arg && is_pointer_type type_) then
    failwith "Cast arguments must be pointers";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_ ~instr:(Cast arg_use)

and mk_cast ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_cast_instr ~arg ~type_)

and set_trunc_instr ~(value : Value.t) ~(arg : Value.t) ~(type_ : Type.t) : unit =
  let arg_type = type_of_value arg in
  if
    not
      (is_integer_type arg_type
      && is_integer_type type_
      && size_of_type arg_type >= size_of_type type_)
  then
    failwith
      "Trunc arguments must be inters with type argument having smaller size than value argument";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_ ~instr:(Trunc arg_use)

and mk_blockless_trunc ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_blockless_instr (set_trunc_instr ~arg ~type_)

and mk_trunc ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_trunc_instr ~arg ~type_)

and set_sext_instr ~(value : Value.t) ~(arg : Value.t) ~(type_ : Type.t) : unit =
  let arg_type = type_of_value arg in
  if
    not
      (is_integer_type arg_type
      && is_integer_type type_
      && size_of_type arg_type <= size_of_type type_)
  then
    failwith
      "SExt arguments must be integers with type argument having larger size than value argument";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_ ~instr:(SExt arg_use)

and mk_blockless_sext ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_blockless_instr (set_sext_instr ~arg ~type_)

and mk_sext ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_sext_instr ~arg ~type_)

and set_zext_instr ~(value : Value.t) ~(arg : Value.t) ~(type_ : Type.t) : unit =
  let arg_type = type_of_value arg in
  if
    not
      (is_integer_type arg_type
      && is_integer_type type_
      && size_of_type arg_type <= size_of_type type_)
  then
    failwith
      "ZExt arguments must be integers with type argument having larger size than value argument";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_ ~instr:(ZExt arg_use)

and mk_blockless_zext ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_blockless_instr (set_zext_instr ~arg ~type_)

and mk_zext ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_zext_instr ~arg ~type_)

and set_int_to_float_instr ~(value : Value.t) ~(arg : Value.t) ~(type_ : Type.t) : unit =
  let arg_type = type_of_value arg in
  if not (is_integer_type arg_type && is_float_type type_) then
    failwith "IntToFloat must have integer argument converted to float type";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_ ~instr:(IntToFloat arg_use)

and mk_int_to_float ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_int_to_float_instr ~arg ~type_)

and set_float_to_int_instr ~(value : Value.t) ~(arg : Value.t) ~(type_ : Type.t) : unit =
  let arg_type = type_of_value arg in
  if not (is_float_type arg_type && is_integer_type type_) then
    failwith "IntToFloat must have float argument converted to integer type";
  let arg_use = user_add_use ~user:value ~use:arg in
  set_instr ~value ~type_ ~instr:(FloatToInt arg_use)

and mk_float_to_int ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  mk_block_instr ~block (set_float_to_int_instr ~arg ~type_)

and set_unreachable_instr ~(value : Value.t) : unit =
  set_instr ~value ~type_:no_return_type ~instr:Unreachable

and mk_unreachable_ ~(block : Block.t) : unit =
  mk_block_instr ~block set_unreachable_instr |> ignore

and set_continue_instr ~(value : Value.t) ~(continue : Block.t) : unit =
  set_instr ~value ~type_:no_return_type ~instr:(Continue continue)

and mk_continue_ ~(block : Block.t) ~(continue : Block.t) : unit =
  mk_block_instr ~block (set_continue_instr ~continue) |> ignore

and set_branch_instr ~(value : Value.t) ~(test : Value.t) ~(continue : Block.t) ~(jump : Block.t) :
    unit =
  let test_use = user_add_use ~user:value ~use:test in
  set_instr ~value ~type_:no_return_type ~instr:(Branch { test = test_use; continue; jump })

and mk_branch_ ~(block : Block.t) ~(test : Value.t) ~(continue : Block.t) ~(jump : Block.t) : unit =
  mk_block_instr ~block (set_branch_instr ~test ~continue ~jump) |> ignore

(*
 * ============================
 *           Globals
 * ============================
 *)
and mk_global
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

and global_set_init ~(global : Global.t) ~(init : Value.t option) =
  (* TODO: Check if the old and new value are the same *)
  (match global.init_val with
  | None -> ()
  | Some old_init_use -> remove_use old_init_use);
  let init_use = Option.map (fun init -> user_add_use ~user:global.value ~use:init) init in
  global.init_val <- init_use

(*
 * ============================
 *          Functions
 * ============================
 *)
and mk_function ~(name : label) : Function.t =
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

and mk_argument ~(func : Function.t) ~(decl_loc : Loc.t) ~(type_ : Type.t) : Value.t =
  let argument = { Argument.type_; func; decl_loc } in
  mk_value (Argument argument)

and func_iter_blocks (func : Function.t) (f : Block.t -> unit) = BlockSet.iter f func.blocks

(*
 * ============================
 *            Uses
 * ============================
 *)
and add_use_link (u1 : Use.t) (u2 : Use.t) =
  u1.next <- u2;
  u2.prev <- u1

and add_value_use ~(value : Value.t) ~(use : Use.t) =
  match value.uses with
  | None ->
    value.uses <- Some use;
    add_use_link use use
  | Some first_use ->
    add_use_link use first_use.next;
    add_use_link first_use use

and user_add_use ~(user : Value.t) ~(use : Value.t) =
  let rec use_node = { Use.value = use; prev = use_node; next = use_node; user } in
  add_value_use ~value:use ~use:use_node;
  use_node

and remove_use (use : Use.t) =
  let value = use.value in
  if use.next == use then
    value.uses <- None
  else
    let prev = use.prev in
    let next = use.next in
    add_use_link prev next;
    match value.uses with
    | Some first_use when first_use == use -> value.uses <- Some use.next
    | _ -> ()

and value_has_uses (value : Value.t) : bool = value.uses <> None

and value_has_single_use (value : Value.t) : bool =
  match value.uses with
  | Some first_use when first_use.next == first_use -> true
  | _ -> false

and is_single_use (use : Use.t) : bool = use.next == use

and value_iter_uses ~(value : Value.t) (f : Use.t -> unit) =
  match value.uses with
  | None -> ()
  | Some first_use ->
    let rec iter current_use =
      let next_use = current_use.Use.next in
      f current_use;
      if next_use != first_use then iter next_use
    in
    iter first_use

and value_get_uses ~(value : Value.t) : Use.t list =
  match value.uses with
  | None -> []
  | Some first_use ->
    let rec gather current_use acc =
      let next_use = current_use.Use.next in
      let acc = current_use :: acc in
      if next_use != first_use then
        gather next_use acc
      else
        acc
    in
    gather first_use []

(* Replace all uses of a value with another value. Uses are modified in place and all use links
   are updated appropriately. *)
and value_replace_uses ~(from : Value.t) ~(to_ : Value.t) =
  value_iter_uses ~value:from (fun use ->
      (* Change the value for this use in-place and attach to use list of new value *)
      use.value <- to_;
      add_value_use ~value:to_ ~use);
  from.uses <- None

(*
 * ============================
 *        Instructions
 * ============================
 *)
and instruction_iter_operands ~(instr : Instruction.t) (f : Use.t -> unit) =
  match instr.instr with
  | StackAlloc _
  | Continue _
  | Unreachable ->
    ()
  | Load operand
  | Unary (_, operand)
  | Cast operand
  | Trunc operand
  | SExt operand
  | ZExt operand
  | IntToFloat operand
  | FloatToInt operand
  | Mov operand
  | Branch { test = operand; _ } ->
    f operand
  | Store (operand1, operand2)
  | Binary (_, operand1, operand2)
  | Cmp (_, operand1, operand2) ->
    f operand1;
    f operand2
  | Ret operand_opt -> Option.iter f operand_opt
  | Phi { args } -> BlockMap.iter (fun _ use -> f use) args
  | Call { func; args; _ } ->
    (match func with
    | Value value -> f value
    | MirBuiltin _ -> ());
    List.iter f args
  | GetPointer { pointer; pointer_offset; offsets } ->
    f pointer;
    Option.iter f pointer_offset;
    List.iter
      (fun offset ->
        match offset with
        | Instruction.GetPointer.PointerIndex operand -> f operand
        | FieldIndex _ -> ())
      offsets

(*
 * ============================
 *           Blocks
 * ============================
 *)

and mk_block ~(func : Function.t) : Block.t =
  let block =
    { Block.id = Block.mk_id (); func; instructions = None; prev_blocks = BlockSet.empty }
  in
  func.blocks <- BlockSet.add block func.blocks;
  block

(*
 * ============================
 *      Block Instructions
 * ============================
 *)
and has_single_instruction (block : Block.t) : bool =
  match block.instructions with
  | Some { first; last } when first == last -> true
  | _ -> false

and add_instr_link (instr_val1 : Value.t) (instr_val2 : Value.t) =
  let instr1 = cast_to_instruction instr_val1 in
  let instr2 = cast_to_instruction instr_val2 in
  instr1.next <- instr_val2;
  instr2.prev <- instr_val1

(* Prepend an instruction to the beginning of a block's instruction list *)
and prepend_instruction (block : Block.t) (instr_val : Value.t) =
  let instr = cast_to_instruction instr_val in
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr_val; last = instr_val }
  | Some ({ first; last } as list) ->
    add_instr_link instr_val first;
    add_instr_link last instr_val;
    list.first <- instr_val

(* Append an instruction to the end of a block's instruction list *)
and append_instruction (block : Block.t) (instr_val : Value.t) =
  let instr = cast_to_instruction instr_val in
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr_val; last = instr_val }
  | Some ({ first; last } as list) ->
    add_instr_link last instr_val;
    add_instr_link instr_val first;
    list.last <- instr_val

(* Insert an instruction immediately before another instruction in a block's instruction list *)
and insert_instruction_before ~(before : Value.t) (instr_val : Value.t) =
  let before_instr = cast_to_instruction before in
  let instr = cast_to_instruction instr_val in
  let block = before_instr.block in
  instr.block <- block;
  match block.instructions with
  | None -> failwith "Block must have before instruction"
  | Some list ->
    let prev_instr = before_instr.prev in
    add_instr_link prev_instr instr_val;
    add_instr_link instr_val before;
    if list.first == before then list.first <- instr_val

(* Remove an instruction. This removes the instruction from a block's instruction list, and
   removes the uses that correspond to each operand of this instruction. *)
and remove_instruction (instr_val : Value.t) =
  let instr = cast_to_instruction instr_val in
  let block = instr.block in

  instruction_iter_operands ~instr remove_use;
  value_iter_uses ~value:instr_val (fun use ->
      (* A phi can appear in its own arguments, in which case it is already being removed *)
      if instr_val != use.value then
        match use.value.value with
        | Instr { instr = Phi phi; _ } ->
          phi_filter_args ~phi (fun _ arg_use -> arg_use.Use.value != instr_val)
        | _ -> ());

  (* Instruction list is circular, so check if single element list *)
  if instr.next == instr_val then
    block.instructions <- None
  else
    let prev = instr.prev in
    let next = instr.next in
    add_instr_link prev next;
    let list = Option.get block.instructions in
    if list.first == instr_val then list.first <- next;
    if list.last == instr_val then list.last <- prev

(* Concatenate the instructions in the second block to the end of the first block.
   This is a destructive operation on the second block's instructions. Removes the first block's
   terminator instruction. *)
and concat_instructions (b1 : Block.t) (b2 : Block.t) =
  (* Remove terminator from first block *)
  (match get_terminator_value b1 with
  | Some terminator -> remove_instruction terminator
  | None -> ());
  (* Concatenate lists of instructions *)
  iter_instructions b2 (fun _ instr -> instr.Instruction.block <- b1);
  match (b1.instructions, b2.instructions) with
  | (_, None) -> ()
  | (None, (Some _ as instrs)) -> b1.instructions <- instrs
  | (Some ({ first = first1; last = last1 } as list), Some { first = first2; last = last2 }) ->
    add_instr_link last1 first2;
    add_instr_link last2 first1;
    list.last <- last2

(* Split a block after an instruction into two separate blocks with no continue between them.
   Return a tuple of the (first block, second block). *)
and split_block_after_instruction (instr_value : Value.t) : Block.t * Block.t =
  let instr = cast_to_instruction instr_value in
  let first_block = instr.block in
  let second_block = mk_block ~func:first_block.func in
  let { Block.first = first_block_first; last = first_block_last } =
    Option.get first_block.instructions
  in

  (* Next blocks now have the second block as their prev block *)
  BlockSet.iter
    (fun next_block ->
      remove_block_link first_block next_block;
      add_block_link second_block next_block;
      map_phi_backreferences_for_block
        ~block:next_block
        ~from:first_block
        ~to_:(BlockSet.singleton second_block))
    (get_next_blocks first_block);

  (* If instruction is at end of block then first block is unchanged and second block is empty *)
  if instr_value == first_block_last then
    (first_block, second_block)
  else
    let instr_next = instr.next in

    (* Create circular linked lists of instructions for first and second blocks *)
    add_instr_link instr_value first_block_first;
    first_block.instructions <- Some { first = first_block_first; last = instr_value };

    add_instr_link first_block_last instr_next;
    second_block.instructions <- Some { first = instr_next; last = first_block_last };
    iter_instructions second_block (fun _ instr -> instr.block <- second_block);

    (first_block, second_block)

(* Replace an instruction with another value, removing the instruction and replacing all its
   uses with the other value. *)
and replace_instruction ~(from : Value.t) ~(to_ : Value.t) =
  value_replace_uses ~from ~to_;
  remove_instruction from

and iter_instructions (block : Block.t) (f : Value.t -> Instruction.t -> unit) =
  match block.instructions with
  | None -> ()
  | Some { first; last } ->
    let rec iter current_val last_val f =
      (* Save next in case instruction is modified *)
      let current = cast_to_instruction current_val in
      let next = current.next in
      f current_val current;
      if current_val != last_val then iter next last_val f
    in
    iter first last f

(* Return the first instruction that matches the predicate, if such an instruction exists *)
and find_instruction (block : Block.t) (f : Value.t -> Instruction.t -> bool) : Value.t option =
  match block.instructions with
  | None -> None
  | Some { first; last } ->
    let rec iter current_val last_val f =
      let current = cast_to_instruction current_val in
      if f current_val current then
        Some current_val
      else if current_val == last_val then
        None
      else
        iter current.next last_val f
    in
    iter first last f

and filter_instructions (block : Block.t) (f : Instruction.t -> bool) =
  iter_instructions block (fun instr_val instr ->
      if not (f instr) then remove_instruction instr_val)

and fold_instructions : 'a. Block.t -> 'a -> (Value.t -> Instruction.t -> 'a -> 'a) -> 'a =
 fun block acc f ->
  match block.instructions with
  | None -> acc
  | Some { first; last } ->
    let rec fold current_val last_val f acc =
      let current = cast_to_instruction current_val in
      let acc' = f current_val current acc in
      if current_val == last_val then
        acc'
      else
        fold current.Instruction.next last_val f acc'
    in
    fold first last f acc

(*
 * ============================
 *         Block Phis
 * ============================
 *)
and block_has_phis (block : Block.t) : bool =
  match block.instructions with
  | Some { first = { value = Instr { instr = Phi _; _ }; _ }; _ } -> true
  | _ -> false

and block_get_phis (block : Block.t) : Instruction.Phi.t list =
  fold_instructions block [] (fun _ instr acc ->
      match instr with
      | { instr = Phi phi; _ } -> phi :: acc
      | _ -> acc)

and block_iter_phis (block : Block.t) (f : Value.t -> Instruction.Phi.t -> unit) =
  iter_instructions block (fun instr_val instr ->
      match instr with
      | { instr = Phi phi; _ } -> f instr_val phi
      | _ -> ())

and block_filter_phis (block : Block.t) (f : Value.id -> Instruction.Phi.t -> bool) =
  iter_instructions block (fun instr_val instr ->
      match instr with
      | { instr = Phi phi; _ } -> if not (f instr_val.id phi) then remove_instruction instr_val
      | _ -> ())

and block_fold_phis (block : Block.t) (acc : 'a) (f : Value.t -> Instruction.Phi.t -> 'a -> 'a) : 'a
    =
  fold_instructions block acc (fun instr_val instr acc ->
      match instr with
      | { instr = Phi phi; _ } -> f instr_val phi acc
      | _ -> acc)

and block_clear_phis (block : Block.t) = block_filter_phis block (fun _ _ -> false)

and phi_add_arg
    ~(phi_val : Value.t) ~(phi : Instruction.Phi.t) ~(block : Block.t) ~(value : Value.t) =
  let use = user_add_use ~user:phi_val ~use:value in
  phi.args <- BlockMap.add block use phi.args

and phi_remove_arg ~(phi : Instruction.Phi.t) ~(block : Block.t) =
  match BlockMap.find_opt block phi.args with
  | None -> ()
  | Some use ->
    remove_use use;
    phi.args <- BlockMap.remove block phi.args

and phi_filter_args ~(phi : Instruction.Phi.t) (f : Block.t -> Use.t -> bool) =
  phi.args <-
    BlockMap.filter
      (fun block use ->
        let keep = f block use in
        if not keep then remove_use use;
        keep)
      phi.args

(* If all phi args have the same value, return that value. Otherwise return None. *)
and phi_get_single_arg_value_with_mapper ~(map_value : Value.t -> Value.t) (phi : Instruction.Phi.t)
    : Value.t option =
  match BlockMap.choose_opt phi.args with
  | None -> None
  | Some (_, first_use) ->
    let first_arg = map_value first_use.value in
    let has_single_arg_value =
      BlockMap.for_all
        (fun _ arg_use ->
          let arg = map_value arg_use.Use.value in
          values_equal arg first_arg)
        phi.args
    in
    if has_single_arg_value then
      Some first_arg
    else
      None

and phi_get_single_arg_value (phi : Instruction.Phi.t) : Value.t option =
  phi_get_single_arg_value_with_mapper ~map_value:Function_utils.id phi

(*
 * ============================
 *        Block Graph
 * ============================
 *)
and add_block_link (prev_block : Block.t) (next_block : Block.t) =
  next_block.prev_blocks <- BlockSet.add prev_block next_block.prev_blocks

and remove_block_link (prev_block : Block.t) (next_block : Block.t) =
  next_block.prev_blocks <- BlockSet.remove prev_block next_block.prev_blocks

(* Return the set of all blocks that this block branches to *)
and get_next_blocks (block : Block.t) : BlockSet.t =
  match get_terminator block with
  | Some { instr = Continue continue; _ } -> BlockSet.singleton continue
  | Some { instr = Branch { test = _; jump; continue }; _ } ->
    BlockSet.add jump (BlockSet.singleton continue)
  | _ -> BlockSet.empty

and iter_next_blocks (block : Block.t) (f : Block.t -> unit) =
  match get_terminator block with
  | Some { instr = Continue continue; _ } -> f continue
  | Some { instr = Branch { test = _; jump; continue }; _ } ->
    f continue;
    f jump
  | _ -> ()

(*
 * ============================
 *     Block Graph Mutation
 * ============================
 *)
and block_remove_if_unreachable ~(on_removed_block : Block.t -> unit) (block : Block.t) =
  if BlockSet.is_empty block.prev_blocks && block.func.start_block != block then
    remove_block ~on_removed_block block

and block_remove_if_empty (block : Block.t) =
  if can_remove_empty_block block then remove_block block

(* An empty block can be removed only if it continues to a single block, and is not needed by any
   phi nodes in its succeeding block. *)
and can_remove_empty_block (block : Block.t) =
  has_single_instruction block
  &&
  match get_terminator block with
  | Some { instr = Continue continue_block; _ } ->
    (* A block is needed if any of its previous blocks appear in a phi node of the next block, with
       a different value than the value from this block. A block is also needed if it is the start
       block and the next block has any phi nodes. If we were to remove this block, the value from
       its branch would be lost in the phi node. *)
    let is_start_block = block.func.start_block == block in
    let continue_block_phis = block_get_phis continue_block in
    let block_needed_for_phi =
      (continue_block_phis <> [] && is_start_block)
      || List.exists
           (fun { Instruction.Phi.args; _ } ->
             BlockMap.exists
               (fun prev_block prev_block_arg ->
                 if BlockSet.mem prev_block block.prev_blocks then
                   not (values_equal prev_block_arg.Use.value (BlockMap.find block args).value)
                 else
                   false)
               args)
           continue_block_phis
    in
    (not block_needed_for_phi) && not is_start_block
  | _ -> false

and remove_block ?(on_removed_block : Block.t -> unit = ignore) (block : Block.t) =
  on_removed_block block;
  (* Remove block from function. This may be the first block in the function. If so, update the
     function to point to the next block as the start. *)
  let func = block.func in
  func.blocks <- BlockSet.remove block func.blocks;
  (match get_terminator block with
  | Some { instr = Continue continue_block; _ } ->
    if func.start_block == block then func.start_block <- continue_block
  | _ -> ());

  (match get_terminator block with
  (* Only when removing unreachable blocks from branch pruning, which could include return block.
     Remove any instances of block from previous blocks. *)
  | Some { instr = Unreachable; _ } ->
    BlockSet.iter
      (fun prev_block ->
        match get_terminator prev_block with
        | Some ({ instr = Continue _; _ } as term_instr) -> term_instr.instr <- Unreachable
        | Some ({ instr = Branch { test; continue; jump }; _ } as term_instr) ->
          term_instr.instr <-
            (if continue == block then
              if jump == block then
                Unreachable
              else
                Continue jump
            else
              Continue continue);
          remove_use test
        | _ -> failwith "Previous block must have branching terminator")
      block.prev_blocks
  | Some { instr = Continue next_block; _ } ->
    (* Update phis in next block to reference previous blocks instead of removed block *)
    map_phi_backreferences_for_block ~block:next_block ~from:block ~to_:block.prev_blocks;

    (* Rewrite next of previous blocks to point to next block instead of removed block *)
    BlockSet.iter
      (fun prev_block -> map_next_block prev_block ~from:block ~to_:next_block)
      block.prev_blocks
  | _ -> ());

  (* Remove links between this block and its next blocks and their phis *)
  BlockSet.iter
    (fun next_block ->
      remove_phi_backreferences_for_block ~block:next_block ~to_remove:block;
      remove_block_link block next_block;
      block_remove_if_unreachable ~on_removed_block next_block)
    (get_next_blocks block);

  (* Remove all operand uses in instructions in the block *)
  iter_instructions block (fun _ instr -> instruction_iter_operands ~instr remove_use)

(* Merge adjacent blocks b1 and b2. Must only be called if b1 and b2 can be merged, meaning
   b1 only continues to b2 and b2 has no other previous blocks. *)
and merge_adjacent_blocks block1 block2 =
  let open Block in
  let map_block block =
    if block == block2 then
      block1
    else
      block
  in
  (* Use b2's next, but take care to reference b1 instead of b2 in the case of self references *)
  (match get_terminator block2 with
  | Some ({ instr = Continue continue; _ } as term_instr) ->
    term_instr.instr <- Continue (map_block continue)
  | Some ({ instr = Branch { test; continue; jump }; _ } as term_instr) ->
    term_instr.instr <- Branch { test; continue = map_block continue; jump = map_block jump }
  | _ -> ());
  concat_instructions block1 block2;
  (* References to the b2 block in phi nodes of blocks that succeed b2 should be rewritten
     to now reference b1 instead. *)
  let next_blocks = get_next_blocks block2 in
  BlockSet.iter
    (fun next_block ->
      map_phi_backreferences_for_block
        ~block:next_block
        ~from:block2
        ~to_:(BlockSet.singleton block1))
    next_blocks;
  (* Set prev pointers for blocks that succeed b2 to point to b1 instead *)
  BlockSet.iter
    (fun next_block ->
      remove_block_link block2 next_block;
      add_block_link block1 next_block)
    next_blocks;
  remove_block_link block1 block2;
  (* Remove b2 from remaining maps in context *)
  let func = block2.func in
  func.blocks <- BlockSet.remove block2 func.blocks

and prune_branch (to_keep : bool) (block : Block.t) ~(on_removed_block : Block.t -> unit) =
  match get_terminator block with
  | Some ({ instr = Branch { test; continue; jump }; _ } as terminator_instr) ->
    let (to_continue, to_prune) =
      if to_keep then
        (continue, jump)
      else
        (jump, continue)
    in
    (* Remove block link and set to continue to unpruned block *)
    remove_block_link block to_prune;
    remove_phi_backreferences_for_block ~block:to_prune ~to_remove:block;
    terminator_instr.instr <- Continue to_continue;
    remove_use test;
    (* Pruning a branch may cause other to become unreachable *)
    block_remove_if_unreachable ~on_removed_block to_prune
  | _ -> failwith "Expected branch terminator"

(* Split an edge between two blocks, inserting an empty block in the middle *)
and split_block_edge (prev_block : Block.t) (next_block : Block.t) : Block.t =
  let func = prev_block.func in
  let new_block =
    {
      Block.id = Block.mk_id ();
      func;
      instructions = None;
      prev_blocks = BlockSet.singleton prev_block;
    }
  in
  mk_continue_ ~block:new_block ~continue:next_block;
  add_block_link new_block next_block;
  func.blocks <- BlockSet.add new_block func.blocks;
  map_next_block prev_block ~from:next_block ~to_:new_block;
  map_phi_backreferences_for_block
    ~block:next_block
    ~from:prev_block
    ~to_:(BlockSet.singleton new_block);
  new_block

(* Map block's next block from a block to another block. Do not update any phi references. *)
and map_next_block (block : Block.t) ~(from : Block.t) ~(to_ : Block.t) =
  let map_next_block maybe_from =
    if maybe_from == from then (
      remove_block_link block from;
      add_block_link block to_;
      to_
    ) else
      maybe_from
  in
  match get_terminator block with
  | Some ({ instr = Continue continue; _ } as term_instr) ->
    term_instr.instr <- Continue (map_next_block continue)
  | Some ({ instr = Branch { test; jump; continue }; _ } as term_instr) ->
    let new_continue = map_next_block continue in
    let new_jump = map_next_block jump in
    (* If both branches point to same block convert to continue *)
    term_instr.instr <-
      (if new_continue == new_jump then
        Continue new_continue
      else
        Branch { test; continue = new_continue; jump = new_jump })
  | _ -> ()

(* Remove all references to a block from phi nodes of on of its next blocks.
   This may be needed when removing a block or block link. *)
and remove_phi_backreferences_for_block ~(block : Block.t) ~(to_remove : Block.t) =
  block_iter_phis block (fun _ phi ->
      phi_filter_args ~phi (fun prev_block _ -> prev_block != to_remove))

(* Replace all references to old_block_id in the phis of a block with new_block_ids. Note that there
   may be multiple new_block_ids, so a single phi argument may be expanded to multiple arguments.
   This may be needed when editing the program. *)
and map_phi_backreferences_for_block ~(block : Block.t) ~(from : Block.t) ~(to_ : BlockSet.t) =
  block_iter_phis block (fun phi_val phi ->
      match BlockMap.find_opt from phi.args with
      | None -> ()
      | Some use ->
        phi_remove_arg ~phi ~block:from;
        BlockSet.iter
          (fun to_block -> phi_add_arg ~phi_val ~phi ~block:to_block ~value:use.value)
          to_)

(*
 * ============================
 *           Program
 * ============================
 *)
and program_iter_funcs (program : Program.t) (f : Function.t -> unit) =
  SMap.iter (fun _ func -> f func) program.funcs

and program_iter_blocks (program : Program.t) (f : Block.t -> unit) =
  program_iter_funcs program (fun func -> func_iter_blocks func f)

and program_remove_func ~(program : Program.t) ~(func : Function.t) =
  program.funcs <- SMap.remove func.name program.funcs

(*
 * ============================
 *         Validation
 * ============================
 *)

and assert_valid_program (program : Program.t) =
  SMap.iter (fun _ func -> assert_valid_function func) program.funcs

and assert_valid_function (func : Function.t) =
  assert_valid_function_cfg func;
  func_iter_blocks func (fun block ->
      assert_valid_instruction_list block;
      iter_instructions block (fun instr_value _ -> assert_valid_use_list ~value:instr_value))

and assert_valid_function_cfg (func : Function.t) =
  (* Create multimap of all previous blocks by visiting CFG *)
  let prev_blocks = ref BlockMMap.empty in
  func_iter_blocks func (fun block ->
      BlockSet.iter
        (fun next_block -> prev_blocks := BlockMMap.add next_block block !prev_blocks)
        (get_next_blocks block));

  func_iter_blocks func (fun block ->
      (* Check that prev blocks for each block matches the true CFG *)
      let prev_blocks_1 = block.prev_blocks in
      let prev_blocks_2 = BlockMMap.find_all block !prev_blocks in

      let is_subset_1 =
        BlockSet.for_all (fun block -> BlockMMap.VSet.mem block prev_blocks_2) prev_blocks_1
      in
      let is_subset_2 =
        BlockMMap.VSet.for_all (fun block -> BlockSet.mem block prev_blocks_1) prev_blocks_2
      in

      if (not is_subset_1) || not is_subset_2 then
        failwith "Previous blocks do not match structure of cfg\n";

      (* Check that each phi contains entries for all previous blocks *)
      block_iter_phis block (fun _ phi ->
          let phi_prev_blocks =
            BlockMap.fold (fun block _ acc -> BlockSet.add block acc) phi.args BlockSet.empty
          in
          if not (BlockSet.equal prev_blocks_1 phi_prev_blocks) then
            failwith
              (Printf.sprintf
                 "Phi does not have arguments for all previous blocks for block %s in func %s\n"
                 (Block.id_to_string block.id)
                 func.name)))

(* Utility function to check if a use list has a valid structure *)
and assert_valid_use_list ~(value : Value.t) =
  match value.uses with
  | None -> ()
  | Some first_use ->
    let rec iter current_use last_use =
      let next_use = current_use.Use.next in
      if next_use.prev != current_use then failwith "Link is not bidirectional";
      if current_use.value != value then failwith "Use does not have correct value";
      if next_use != last_use then iter next_use last_use
    in
    iter first_use first_use

(* Utility function to check if an instruction list has a valid structure *)
and assert_valid_instruction_list (block : Block.t) =
  match block.instructions with
  | None -> ()
  | Some { first = first_val; last = last_val } ->
    let first = cast_to_instruction first_val in
    let last = cast_to_instruction last_val in
    if first.prev != last_val || last.next != first_val then
      failwith
        (Printf.sprintf
           "List must be circular %B %B"
           (first.prev != last_val)
           (last.next != first_val));
    let rec iter current_val last_val =
      let current = cast_to_instruction current_val in
      let current_next = cast_to_instruction current.next in
      if current.block != block then failwith "Instruction does not have correct block";
      if current_next.prev != current_val then failwith "Link is not bidirectional";
      if current.next != last_val then iter current.next last_val
    in
    iter first_val last_val
