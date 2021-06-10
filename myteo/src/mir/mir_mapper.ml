open Basic_collections
open Mir
open Immutable_utils
module Ocx = Mir_optimize_context

type current_instruction_edit =
  | Keep
  | Remove
  | Replace of var_id Instruction.t list

module InstructionsMapper = struct
  class t ~(ocx : Ocx.t) =
    object (this)
      val program : ssa_program = ocx.program

      val mutable current_instruction_edit = Keep

      method mark_instruction_removed () = current_instruction_edit <- Remove

      method replace_instruction instrs = current_instruction_edit <- Replace instrs

      method map_instructions ~(block : var_id Block.t) (instructions : var_id Instruction.t list) =
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
        | Ret arg_opt ->
          let arg_opt' = id_map_opt (this#map_value ~block) arg_opt in
          if arg_opt == arg_opt' then
            [instruction]
          else
            mk_instr (Ret arg_opt')
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
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Eq (result', left', right'))
        | Neq (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
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
            mk_instr (GtEq (result', left', right'))
        | GtEq (result, left, right) ->
          let result' = this#map_result_variable ~block result in
          let left' = this#map_numeric_value ~block left in
          let right' = this#map_numeric_value ~block right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (GtEq (result', left', right'))

      method map_result_variable ~block:_ var_id = var_id

      method map_use_variable ~block:_ var_id = var_id

      method map_value ~block value =
        match value with
        | (`UnitL | `UnitV _) as v -> this#map_unit_value ~block v
        | (`BoolL _ | `BoolV _) as v -> (this#map_bool_value ~block v :> ssa_value)
        | (`StringL _ | `StringV _) as v -> this#map_string_value ~block v
        | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v ->
          (this#map_numeric_value ~block v :> ssa_value)
        | (`FunctionL _ | `FunctionV _) as v -> (this#map_function_value ~block v :> ssa_value)
        | (`PointerL _ | `PointerV _) as v -> (this#map_pointer_value ~block v :> ssa_value)
        | `AggregateV _ as v -> this#map_aggregate_value ~block v

      method map_unit_value ~block value =
        match value with
        | `UnitL as value -> value
        | `UnitV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `UnitV var_id')

      method map_bool_value ~block value =
        match value with
        | `BoolL _ as value -> value
        | `BoolV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `BoolV var_id')

      method map_string_value ~block value =
        match value with
        | `StringL _ as value -> value
        | `StringV var_id as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' -> `StringV var_id')

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

      method map_aggregate_value ~block value =
        match value with
        | `AggregateV (agg, var_id) as value ->
          id_map (this#map_use_variable ~block) var_id value (fun var_id' ->
              `AggregateV (agg, var_id'))

      method map_phis ~block (phis : var_id Block.phi list) =
        List.map
          (fun (value_type, var_id, args) ->
            let var_id' = this#map_result_variable ~block var_id in
            let args' = IMap.map (this#map_use_variable ~block) args in
            (value_type, var_id', args'))
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

class rewrite_vars_mapper ~ocx var_map =
  object (this)
    inherit InstructionsMapper.t ~ocx

    method resolve_var var_id =
      let rec rec_resolve var_id =
        match IMap.find_opt var_id var_map with
        | None -> var_id
        | Some new_var_id -> rec_resolve new_var_id
      in
      match IMap.find_opt var_id var_map with
      | None -> None
      | Some new_var_id -> Some (rec_resolve new_var_id)

    method! map_result_variable ~block:_ var_id =
      match this#resolve_var var_id with
      | None -> var_id
      | Some new_var_id -> new_var_id

    method! map_use_variable ~block:_ var_id =
      match this#resolve_var var_id with
      | None -> var_id
      | Some new_var_id -> new_var_id
  end
