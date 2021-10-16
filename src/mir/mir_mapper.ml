open Basic_collections
open Mir
open Immutable_utils
module Ocx = Mir_optimize_context

type current_instruction_edit =
  | Keep
  | Remove
  | Replace of Instruction.t list

module InstructionsMapper = struct
  class t ~(program : Program.t) =
    object (this)
      val program : Program.t = program

      val mutable current_instruction_edit = Keep

      method mark_instruction_removed () = current_instruction_edit <- Remove

      method replace_instruction instrs = current_instruction_edit <- Replace instrs

      method map_function (func : Function.t) =
        let visited_blocks = ref ISet.empty in
        let rec get_block block_id = IMap.find block_id program.blocks
        and check_visited_block block_id =
          if ISet.mem block_id !visited_blocks then
            true
          else (
            visited_blocks := ISet.add block_id !visited_blocks;
            false
          )
        and visit_block (block : Block.t) =
          if check_visited_block block.id then
            ()
          else (
            this#map_block block;
            match block.next with
            | Halt -> ()
            | Continue id -> visit_block (get_block id)
            | Branch { test = _; continue; jump } ->
              visit_block (get_block continue);
              visit_block (get_block jump)
          )
        in
        visit_block (get_block func.body_start_block)

      method map_block (block : Block.t) =
        block.phis <- this#map_phis ~block block.phis;
        block.instructions <- this#map_instructions ~block block.instructions;
        block.next <- this#map_block_next ~block block.next

      method map_instructions ~(block : Block.t) (instructions : Instruction.t list) =
        let instructions' =
          List.filter_map
            (fun instruction ->
              current_instruction_edit <- Keep;
              let instrs = this#map_instruction ~block instruction in
              match current_instruction_edit with
              | Keep -> Some instrs
              | Remove -> None
              | Replace instrs -> Some instrs)
            instructions
        in
        List.flatten instructions'

      method map_instruction ~block ((instr_id, instr) as instruction) =
        let open Instruction in
        let mk_instr instr = [(instr_id, instr)] in
        match instr with
        | Mov (result, arg) ->
          let result' = this#map_result_variable ~block result in
          let arg' = this#map_value ~block arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (Mov (result', arg'))
        | Call (ret, ret_ty, func, args) ->
          let ret' = this#map_result_variable ~block ret in
          let func' = this#map_function_value ~block func in
          let args' = id_map_list (this#map_value ~block) args in
          if ret == ret' && func == func' && args == args' then
            [instruction]
          else
            mk_instr (Call (ret', ret_ty, func', args'))
        | CallBuiltin (ret, ret_ty, builtin, args) ->
          let ret' = this#map_result_variable ~block ret in
          let args' = id_map_list (this#map_value ~block) args in
          if ret == ret' && args == args' then
            [instruction]
          else
            mk_instr (CallBuiltin (ret', ret_ty, builtin, args'))
        | Ret arg_opt ->
          let arg_opt' = id_map_opt (this#map_value ~block) arg_opt in
          if arg_opt == arg_opt' then
            [instruction]
          else
            mk_instr (Ret arg_opt')
        | StackAlloc (result, ty) ->
          let result' = this#map_result_variable ~block result in
          if result == result' then
            [instruction]
          else
            mk_instr (StackAlloc (result', ty))
        | Load (result, ptr) ->
          let result' = this#map_result_variable ~block result in
          let ptr' = this#map_pointer_value ~block ptr in
          if result == result' && ptr == ptr' then
            [instruction]
          else
            mk_instr (Load (result', ptr'))
        | Store (ptr, arg) ->
          let ptr' = this#map_pointer_value ~block ptr in
          let arg' = this#map_value ~block arg in
          if ptr == ptr' && arg == arg' then
            [instruction]
          else
            mk_instr (Store (ptr', arg'))
        | GetPointer { GetPointer.var_id; return_ty; pointer; pointer_offset; offsets } ->
          let var_id' = this#map_result_variable ~block var_id in
          let pointer' = this#map_pointer_value ~block pointer in
          let pointer_offset' = id_map_opt (this#map_numeric_value ~block) pointer_offset in
          let offsets' =
            id_map_list
              (fun offset ->
                match offset with
                | GetPointer.PointerIndex index ->
                  id_map (this#map_numeric_value ~block) index offset (fun index' ->
                      GetPointer.PointerIndex index')
                | GetPointer.FieldIndex _ -> offset)
              offsets
          in
          if
            var_id == var_id'
            && pointer == pointer'
            && pointer_offset == pointer_offset'
            && offsets == offsets'
          then
            [instruction]
          else
            mk_instr
              (GetPointer
                 {
                   GetPointer.var_id = var_id';
                   return_ty;
                   pointer = pointer';
                   pointer_offset = pointer_offset';
                   offsets = offsets';
                 })
        | LogNot (result, arg) ->
          let result' = this#map_result_variable ~block result in
          let arg' = this#map_bool_value ~block arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (LogNot (result', arg'))
        | LogAnd (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_bool_value ~block left in
          let right' = this#map_bool_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (LogAnd (result', left', right'))
        | LogOr (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_bool_value ~block left in
          let right' = this#map_bool_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (LogOr (result', left', right'))
        | BitNot (result, arg) ->
          let result' = this#map_result_variable ~block result in
          let arg' = this#map_numeric_value ~block arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (BitNot (result', arg'))
        | BitAnd (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (BitAnd (result', left', right'))
        | BitOr (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (BitOr (result', left', right'))
        | BitXor (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (BitXor (result', left', right'))
        | Shl (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Shl (result', left', right'))
        | Shr (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Shr (result', left', right'))
        | Shrl (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Shrl (result', left', right'))
        | Neg (result, arg) ->
          let result' = this#map_result_variable ~block result in
          let arg' = this#map_numeric_value ~block arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (Neg (result', arg'))
        | Add (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Add (result', left', right'))
        | Sub (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Sub (result', left', right'))
        | Mul (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Mul (result', left', right'))
        | Div (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Div (result', left', right'))
        | Rem (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Rem (result', left', right'))
        | Eq (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_comparable_value ~block left in
          let right' = this#map_comparable_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Eq (result', left', right'))
        | Neq (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_comparable_value ~block left in
          let right' = this#map_comparable_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Neq (result', left', right'))
        | Lt (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Lt (result', left', right'))
        | LtEq (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (LtEq (result', left', right'))
        | Gt (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Gt (result', left', right'))
        | GtEq (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (GtEq (result', left', right'))
        | Trunc (result, arg, ty) ->
          let result' = this#map_result_variable ~block result in
          let arg' = this#map_numeric_value ~block arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (Trunc (result', arg', ty))
        | SExt (result, arg, ty) ->
          let result' = this#map_result_variable ~block result in
          let arg' = this#map_numeric_value ~block arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (SExt (result', arg', ty))

      method map_result_variable ~block:_ var_id = var_id

      method map_use_variable ~block:_ var_id = var_id

      method map_value ~block value =
        match value with
        | (`UnitL | `UnitV _) as v -> (this#map_unit_value ~block v :> Value.t)
        | (`BoolL _ | `BoolV _) as v -> (this#map_bool_value ~block v :> Value.t)
        | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v ->
          (this#map_numeric_value ~block v :> Value.t)
        | (`FunctionL _ | `FunctionV _) as v -> (this#map_function_value ~block v :> Value.t)
        | (`PointerL _ | `PointerV _) as v -> (this#map_pointer_value ~block v :> Value.t)
        | (`ArrayStringL _ | `ArrayVtableL _ | `ArrayV _) as v -> this#map_array_value ~block v

      method map_unit_value ~block value : Value.unit_value =
        match value with
        | `UnitL as value -> value
        | `UnitV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `UnitV var_id')

      method map_bool_value ~block value =
        match value with
        | `BoolL _ as value -> value
        | `BoolV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `BoolV var_id')

      method map_numeric_value ~block value =
        match value with
        | (`ByteL _ | `IntL _ | `LongL _) as value -> value
        | `ByteV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `ByteV var_id')
        | `IntV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `IntV var_id')
        | `LongV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `LongV var_id')

      method map_function_value ~block value =
        match value with
        | `FunctionL _ as value -> value
        | `FunctionV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `FunctionV var_id')

      method map_pointer_value ~block value =
        match value with
        | `PointerL _ as value -> value
        | `PointerV (ty, var_id) as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' ->
              `PointerV (ty, var_id'))

      method map_array_value ~block value =
        match value with
        | (`ArrayStringL _ | `ArrayVtableL _) as value -> value
        | `ArrayV (ty, size, var_id) as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' ->
              `ArrayV (ty, size, var_id'))

      method map_comparable_value ~block value =
        match value with
        | (`UnitL | `UnitV _) as v -> (this#map_unit_value ~block v :> Value.comparable_value)
        | (`BoolL _ | `BoolV _) as v -> (this#map_bool_value ~block v :> Value.comparable_value)
        | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v ->
          (this#map_numeric_value ~block v :> Value.comparable_value)
        | (`PointerL _ | `PointerV _) as v ->
          (this#map_pointer_value ~block v :> Value.comparable_value)

      method map_phis ~block (phis : Block.phi list) =
        List.filter_map
          (fun (value_type, var_id, args) ->
            current_instruction_edit <- Keep;
            let var_id' = this#map_result_variable ~block var_id in
            let args' = IMap.map (this#map_value ~block) args in
            if current_instruction_edit = Remove then
              None
            else
              Some (value_type, var_id', args'))
          phis

      method map_block_next ~block next =
        let open Block in
        match next with
        | Halt
        | Continue _ ->
          next
        | Branch ({ test; _ } as branch) ->
          Branch { branch with test = this#map_bool_value ~block test }
    end
end

class rewrite_vars_mapper ~(program : Program.t) (var_map : Value.t IMap.t) =
  object (this)
    inherit InstructionsMapper.t ~program

    method! map_unit_value ~block value =
      match value with
      | `UnitL -> value
      | `UnitV var_id as value ->
        (match IMap.find_opt var_id var_map with
        | None -> value
        | Some mapped_value -> this#map_unit_value ~block (cast_to_unit_value mapped_value))

    method! map_bool_value ~block value =
      match value with
      | `BoolL _ -> value
      | `BoolV var_id as value ->
        (match IMap.find_opt var_id var_map with
        | None -> value
        | Some mapped_value -> this#map_bool_value ~block (cast_to_bool_value mapped_value))

    method! map_numeric_value ~block value =
      match value with
      | (`ByteL _ | `IntL _ | `LongL _) as value -> value
      | (`ByteV var_id as value)
      | (`IntV var_id as value)
      | (`LongV var_id as value) ->
        (match IMap.find_opt var_id var_map with
        | None -> value
        | Some mapped_value -> this#map_numeric_value ~block (cast_to_numeric_value mapped_value))

    method! map_function_value ~block value =
      match value with
      | `FunctionL _ as value -> value
      | `FunctionV var_id as value ->
        (match IMap.find_opt var_id var_map with
        | None -> value
        | Some mapped_value -> this#map_function_value ~block (cast_to_function_value mapped_value))

    method! map_pointer_value ~block value =
      match value with
      | `PointerL _ as value -> value
      | `PointerV (element_type, var_id) as value ->
        (match IMap.find_opt var_id var_map with
        | None -> value
        | Some mapped_value ->
          (* Pointers may be cast to a different element type, so preserve element type of original pointer *)
          let mapped_ptr_val = this#map_pointer_value ~block (cast_to_pointer_value mapped_value) in
          cast_pointer_value mapped_ptr_val element_type)

    method! map_array_value ~block value =
      match value with
      | (`ArrayStringL _ | `ArrayVtableL _) as value -> value
      | `ArrayV (_, _, var_id) as value ->
        (match IMap.find_opt var_id var_map with
        | None -> value
        | Some mapped_value -> this#map_array_value ~block (cast_to_array_value mapped_value))
  end
