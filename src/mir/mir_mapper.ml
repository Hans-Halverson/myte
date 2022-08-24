open Mir
open Mir_builders
open Mir_type

let map_instruction ~mapper (instr_value : Value.t) =
  let instr = cast_to_instruction instr_value in

  let map_block block = mapper#map_block block in
  let map_value value = mapper#map_value value in
  let map_use use = map_value use.Use.value in

  let block = map_block instr.block in
  let type_ = instr.type_ in
  let value = map_value instr_value in
  match instr.instr with
  | Phi { args } ->
    let args =
      BlockMap.fold
        (fun source_block source_use args ->
          BlockMap.add (map_block source_block) (map_use source_use) args)
        args
        BlockMap.empty
    in
    set_phi_instr ~value ~type_ ~args
  | Call { func; args; has_return } ->
    let args = List.map map_use args in
    let return =
      if has_return then
        Some type_
      else
        None
    in
    (match func with
    | Value func_use ->
      let func = map_use func_use in
      set_call_instr ~value ~func ~args ~return
    | MirBuiltin builtin -> set_call_builtin_instr ~value ~builtin ~args ~return)
  | StackAlloc type_ -> set_stack_alloc_instr ~value ~type_
  | Load ptr ->
    let ptr = map_use ptr in
    set_load_instr ~value ~ptr
  | Store (ptr, stored_value) ->
    let ptr = map_use ptr in
    let stored_value = map_use stored_value in
    set_store_instr ~instr_value:value ~ptr ~stored_value
  | GetPointer { pointer; pointer_offset; offsets } ->
    let pointer = map_use pointer in
    let pointer_offset = Option.map map_use pointer_offset in
    let offsets =
      List.map
        (fun offset ->
          let open Instruction.GetPointer in
          match offset with
          | PointerIndex index -> PointerIndex (map_use index)
          | FieldIndex index -> FieldIndex index)
        offsets
    in
    let type_ = cast_to_pointer_type type_ in
    set_get_pointer_instr ~value ~type_ ~ptr:pointer ~pointer_offset ~offsets ()
  | Unary (op, arg) ->
    let arg = map_use arg in
    set_unary_instr ~value ~op ~arg
  | Binary (op, left, right) ->
    let left = map_use left in
    let right = map_use right in
    set_binary_instr ~value ~op ~left ~right
  | Cmp (cmp, left, right) ->
    let left = map_use left in
    let right = map_use right in
    set_cmp_instr ~value ~cmp ~left ~right
  | Cast arg ->
    let arg = map_use arg in
    set_cast_instr ~value ~arg ~type_
  | Trunc arg ->
    let arg = map_use arg in
    set_trunc_instr ~value ~arg ~type_
  | SExt arg ->
    let arg = map_use arg in
    set_sext_instr ~value ~arg ~type_
  | ZExt arg ->
    let arg = map_use arg in
    set_zext_instr ~value ~arg ~type_
  | IntToFloat arg ->
    let arg = map_use arg in
    set_int_to_float_instr ~value ~arg ~type_
  | FloatToInt arg ->
    let arg = map_use arg in
    set_float_to_int_instr ~value ~arg ~type_
  | Unreachable -> set_unreachable_instr ~value
  | Ret arg ->
    let arg = Option.map map_use arg in
    set_ret_instr ~value ~arg
  | Continue continue ->
    let continue = map_block continue in
    set_continue_instr ~value ~continue;
    add_block_link block continue
  | Branch { test; continue; jump } ->
    let test = map_use test in
    let continue = map_block continue in
    let jump = map_block jump in
    set_branch_instr ~value ~test ~continue ~jump;
    add_block_link block continue;
    add_block_link block jump
  | Mov arg ->
    let arg = map_use arg in
    set_mov_instr ~value ~arg

class id_mapper =
  object
    method map_value (value : Value.t) = value
    method map_block (block : Block.t) = block
  end

let id_mapper = new id_mapper
