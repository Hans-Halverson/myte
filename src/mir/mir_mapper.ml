open Basic_collections
open Mir
open Immutable_utils
module Ocx = Mir_optimize_context

type current_instruction_edit =
  | Keep
  | Remove

module InstructionsMapper = struct
  class t ~(program : Program.t) =
    object (this)
      val program : Program.t = program

      val mutable current_instruction_edit = Keep

      method mark_instruction_removed () = current_instruction_edit <- Remove

      method map_function (func : Function.t) = BlockSet.iter this#map_block func.blocks

      method map_block (block : Block.t) = this#map_instructions block

      method map_instructions (block : Block.t) =
        iter_instructions block (fun instruction ->
            current_instruction_edit <- Keep;
            this#map_instruction instruction;
            match current_instruction_edit with
            | Keep -> ()
            | Remove -> remove_instruction block instruction)

      method map_instruction (instruction : Instruction.t) =
        let open Instruction in
        match instruction.instr with
        | Mov arg ->
          let arg' = this#map_use arg in
          if arg != arg' then instruction.instr <- Mov arg'
        | Phi ({ args } as phi) -> phi.args <- BlockMap.map this#map_use args
        | Call { func; args; has_return } ->
          let func' =
            match func with
            | Value v -> id_map this#map_use v func (fun v' -> Call.Value v')
            | MirBuiltin _ -> func
          in
          let args' = id_map_list this#map_use args in
          if func != func' || args != args' then
            instruction.instr <- Call { func = func'; args = args'; has_return }
        | Ret arg_opt ->
          let arg_opt' = id_map_opt this#map_use arg_opt in
          if arg_opt != arg_opt' then instruction.instr <- Ret arg_opt'
        | StackAlloc _ty -> ()
        | Load ptr ->
          let ptr' = this#map_use ptr in
          if ptr != ptr' then instruction.instr <- Load ptr'
        | Store (ptr, arg) ->
          let ptr' = this#map_use ptr in
          let arg' = this#map_use arg in
          if ptr != ptr' || arg != arg' then instruction.instr <- Store (ptr', arg')
        | GetPointer { GetPointer.pointer; pointer_offset; offsets } ->
          let pointer' = this#map_use pointer in
          let pointer_offset' = id_map_opt this#map_use pointer_offset in
          let offsets' =
            id_map_list
              (fun offset ->
                match offset with
                | GetPointer.PointerIndex index ->
                  id_map this#map_use index offset (fun index' -> GetPointer.PointerIndex index')
                | GetPointer.FieldIndex _ -> offset)
              offsets
          in
          if pointer != pointer' || pointer_offset != pointer_offset' || offsets != offsets' then
            instruction.instr <-
              GetPointer
                {
                  GetPointer.pointer = pointer';
                  pointer_offset = pointer_offset';
                  offsets = offsets';
                }
        | Unary (op, arg) ->
          let arg' = this#map_use arg in
          if arg != arg' then instruction.instr <- Unary (op, arg')
        | Binary (op, left, right) ->
          let left' = this#map_use left in
          let right' = this#map_use right in
          if left != left' || right != right' then instruction.instr <- Binary (op, left', right')
        | Cmp (cmp, left, right) ->
          let left' = this#map_use left in
          let right' = this#map_use right in
          if left != left' || right != right' then instruction.instr <- Cmp (cmp, left', right')
        | Cast arg ->
          let arg' = this#map_use arg in
          if arg != arg' then instruction.instr <- Cast arg'
        | Trunc arg ->
          let arg' = this#map_use arg in
          if arg != arg' then instruction.instr <- Trunc arg'
        | SExt arg ->
          let arg' = this#map_use arg in
          if arg != arg' then instruction.instr <- SExt arg'
        | Unreachable -> ()
        | Continue _ -> ()
        | Branch { test; jump; continue } ->
          let test' = this#map_use test in
          if test != test' then instruction.instr <- Branch { test = test'; jump; continue }

      method map_use (use : Use.t) : Use.t =
        let value' = this#map_value use.value in
        if use.value == value' then
          use
        else
          let use' = Mir_builders.user_add_use ~user:use.user ~use:value' in
          Mir_builders.remove_use ~use;
          use'

      method map_value (value : Value.t) : Value.t = value
    end
end

class rewrite_vals_mapper ~(program : Program.t) (value_map : Value.t IMap.t) =
  object (this)
    inherit InstructionsMapper.t ~program

    method! map_value value =
      match value.value with
      | Lit _ -> value
      | Argument { id; _ }
      | Instr { id; _ } ->
        (match IMap.find_opt id value_map with
        | None -> value
        | Some mapped_value -> this#map_value mapped_value)
  end
