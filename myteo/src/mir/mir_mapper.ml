open Mir
open Immutable_utils
module Ocx = Mir_optimize_context

module InstructionsMapper = struct
  class t ~(ocx : Ocx.t) =
    object (this)
      val program : ssa_program = ocx.program

      (* Set to true to remove the current instruction *)
      val mutable remove_flag : bool = false

      method mark_instruction_removed () = remove_flag <- true

      method map_instructions ~(block : var_id Block.t) (instructions : var_id Instruction.t list) =
        let instructions' =
          List.filter_map
            (fun instruction ->
              remove_flag <- false;
              let instrs = this#map_instruction ~block instruction in
              if remove_flag then
                None
              else
                Some instrs)
            instructions
        in
        List.flatten instructions'

      method map_instruction ~block ((instr_id, instr) as instruction) =
        let open Instruction in
        let mk_instr instr = [(instr_id, instr)] in
        match instr with
        | Mov (result, arg) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let arg' = this#map_value ~block ~instruction arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (Mov (result', arg'))
        | Call (ret, func, args) ->
          let ret' = this#map_result_variable ~block ~instruction ret in
          let func' = this#map_function_value ~block ~instruction func in
          let args' = id_map_list (this#map_value ~block ~instruction) args in
          if ret == ret' && func == func' && args == args' then
            [instruction]
          else
            mk_instr (Call (ret', func', args'))
        | Ret arg_opt ->
          let arg_opt' = id_map_opt (this#map_value ~block ~instruction) arg_opt in
          if arg_opt == arg_opt' then
            [instruction]
          else
            mk_instr (Ret arg_opt')
        | LoadGlobal (result, name) ->
          let result' = this#map_result_variable ~block ~instruction result in
          if result == result' then
            [instruction]
          else
            mk_instr (LoadGlobal (result', name))
        | StoreGlobal (name, arg) ->
          let arg' = this#map_value ~block ~instruction arg in
          if arg == arg' then
            [instruction]
          else
            mk_instr (StoreGlobal (name, arg'))
        | LogNot (result, arg) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let arg' = this#map_bool_value ~block ~instruction arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (LogNot (result', arg'))
        | LogAnd (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_bool_value ~block ~instruction left in
          let right' = this#map_bool_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (LogAnd (result', left', right'))
        | LogOr (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_bool_value ~block ~instruction left in
          let right' = this#map_bool_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (LogOr (result', left', right'))
        | Neg (result, arg) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let arg' = this#map_numeric_value ~block ~instruction arg in
          if result == result' && arg == arg' then
            [instruction]
          else
            mk_instr (Neg (result', arg'))
        | Add (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Add (result', left', right'))
        | Sub (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Sub (result', left', right'))
        | Mul (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Mul (result', left', right'))
        | Div (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Div (result', left', right'))
        | Eq (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Eq (result', left', right'))
        | Neq (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Neq (result', left', right'))
        | Lt (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (Lt (result', left', right'))
        | LtEq (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (LtEq (result', left', right'))
        | Gt (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (GtEq (result', left', right'))
        | GtEq (result, left, right) ->
          let result' = this#map_result_variable ~block ~instruction result in
          let left' = this#map_numeric_value ~block ~instruction left in
          let right' = this#map_numeric_value ~block ~instruction right in
          if result == result' && left == left' && right == right' then
            [instruction]
          else
            mk_instr (GtEq (result', left', right'))

      method map_result_variable ~block:_ ~instruction:_ var_id = var_id

      method map_use_variable ~block:_ ~instruction:_ var_id = var_id

      method map_value ~block ~instruction value =
        match value with
        | Unit v -> id_map (this#map_unit_value ~block ~instruction) v value (fun v' -> Unit v')
        | Bool v -> id_map (this#map_bool_value ~block ~instruction) v value (fun v' -> Bool v')
        | String v ->
          id_map (this#map_string_value ~block ~instruction) v value (fun v' -> String v')
        | Numeric v ->
          id_map (this#map_numeric_value ~block ~instruction) v value (fun v' -> Numeric v')
        | Function v ->
          id_map (this#map_function_value ~block ~instruction) v value (fun v' -> Function v')

      method map_unit_value ~block ~instruction value =
        match value with
        | Lit -> value
        | Var var_id ->
          id_map (this#map_use_variable ~block ~instruction) var_id value (fun var_id' ->
              Var var_id')

      method map_bool_value ~block ~instruction value =
        match value with
        | Lit _ -> value
        | Var var_id ->
          id_map (this#map_use_variable ~block ~instruction) var_id value (fun var_id' ->
              Var var_id')

      method map_string_value ~block ~instruction value =
        match value with
        | Lit _ -> value
        | Var var_id ->
          id_map (this#map_use_variable ~block ~instruction) var_id value (fun var_id' ->
              Var var_id')

      method map_numeric_value ~block ~instruction value =
        match value with
        | IntLit _ -> value
        | IntVar var_id ->
          id_map (this#map_use_variable ~block ~instruction) var_id value (fun var_id' ->
              IntVar var_id')

      method map_function_value ~block ~instruction value =
        match value with
        | Lit _ -> value
        | Var var_id ->
          id_map (this#map_use_variable ~block ~instruction) var_id value (fun var_id' ->
              Var var_id')
    end
end
