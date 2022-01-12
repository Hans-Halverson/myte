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
        block.instructions <- this#map_instructions block.instructions;
        block.next <- this#map_block_next block.next

      method map_block_next next =
        let open Block in
        match next with
        | Halt
        | Continue _ ->
          next
        | Branch ({ test; _ } as branch) -> Branch { branch with test = this#map_value test }

      method map_instructions (instructions : Instruction.t list) =
        let instructions' =
          List.filter_map
            (fun instruction ->
              current_instruction_edit <- Keep;
              let instrs = this#map_instruction instruction in
              match current_instruction_edit with
              | Keep -> Some instrs
              | Remove -> None
              | Replace instrs -> Some instrs)
            instructions
        in
        List.flatten instructions'

      method map_instruction instruction =
        let open Instruction in
        let mk_instr instr = [{ instruction with instr }] in
        match instruction.instr with
        | Mov arg ->
          let arg' = this#map_value arg in
          if arg == arg' then
            [instruction]
          else
            mk_instr (Mov arg')
        | Phi ({ args } as phi) ->
          phi.args <- IMap.map this#map_value args;
          [instruction]
        | Call { func; args; has_return } ->
          let func' =
            match func with
            | Value v -> id_map this#map_value v func (fun v' -> Call.Value v')
            | Builtin _ -> func
          in
          let args' = id_map_list this#map_value args in
          if func == func' && args == args' then
            [instruction]
          else
            mk_instr (Call { func = func'; args = args'; has_return })
        | Ret arg_opt ->
          let arg_opt' = id_map_opt this#map_value arg_opt in
          if arg_opt == arg_opt' then
            [instruction]
          else
            mk_instr (Ret arg_opt')
        | StackAlloc _ty -> [instruction]
        | Load ptr ->
          let ptr' = this#map_value ptr in
          if ptr == ptr' then
            [instruction]
          else
            mk_instr (Load ptr')
        | Store (ptr, arg) ->
          let ptr' = this#map_value ptr in
          let arg' = this#map_value arg in
          if ptr == ptr' && arg == arg' then
            [instruction]
          else
            mk_instr (Store (ptr', arg'))
        | GetPointer { GetPointer.pointer; pointer_offset; offsets } ->
          let pointer' = this#map_value pointer in
          let pointer_offset' = id_map_opt this#map_value pointer_offset in
          let offsets' =
            id_map_list
              (fun offset ->
                match offset with
                | GetPointer.PointerIndex index ->
                  id_map this#map_value index offset (fun index' -> GetPointer.PointerIndex index')
                | GetPointer.FieldIndex _ -> offset)
              offsets
          in
          if pointer == pointer' && pointer_offset == pointer_offset' && offsets == offsets' then
            [instruction]
          else
            mk_instr
              (GetPointer
                 {
                   GetPointer.pointer = pointer';
                   pointer_offset = pointer_offset';
                   offsets = offsets';
                 })
        | Unary (op, arg) ->
          let arg' = this#map_value arg in
          if arg == arg' then
            [instruction]
          else
            mk_instr (Unary (op, arg'))
        | Binary (op, left, right) ->
          let left' = this#map_value left in
          let right' = this#map_value right in
          if left == left' && right == right' then
            [instruction]
          else
            mk_instr (Binary (op, left', right'))
        | Cmp (cmp, left, right) ->
          let left' = this#map_value left in
          let right' = this#map_value right in
          if left == left' && right == right' then
            [instruction]
          else
            mk_instr (Cmp (cmp, left', right'))
        | Cast arg ->
          let arg' = this#map_value arg in
          if arg == arg' then
            [instruction]
          else
            mk_instr (Cast arg')
        | Trunc arg ->
          let arg' = this#map_value arg in
          if arg == arg' then
            [instruction]
          else
            mk_instr (Trunc arg')
        | SExt arg ->
          let arg' = this#map_value arg in
          if arg == arg' then
            [instruction]
          else
            mk_instr (SExt arg')

      method map_value value = value
    end
end

class rewrite_vals_mapper ~(program : Program.t) (value_map : Value.t IMap.t) =
  object (this)
    inherit InstructionsMapper.t ~program

    method! map_value value =
      match value with
      | Lit _ -> value
      | Argument { id; _ }
      | Instr { id; _ } ->
        (match IMap.find_opt id value_map with
        | None -> value
        | Some mapped_value -> this#map_value mapped_value)
  end
