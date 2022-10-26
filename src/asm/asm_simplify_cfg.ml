open Asm
open Asm_builders

class virtual compress_jump_aliases ~(func : Function.t) =
  object (this)
    val mutable jump_aliases : Block.t BlockMap.t = BlockMap.empty

    (* Return the block that this instruction unconditionally branches to, if one exists *)
    method virtual get_unconditional_branch_block : Instruction.t -> Block.t option

    (* Call a function on all block operands in this function *)
    method virtual iter_all_block_operands : (Operand.t -> unit) -> unit

    (* A callback to run on all removed blocks *)
    method virtual on_removed_block : Block.t -> unit

    (* Find all empty blocks that only contain an unconditional jump. Remove them and rewrite other
       jumps in graph to skip over them (may skip an entire chain). *)
    method run () =
      let open Block in
      (* Find all jump aliases in program *)
      jump_aliases <- BlockMap.empty;
      func_iter_blocks func (fun block ->
          if has_single_instruction block then
            match get_first_instr_opt block with
            | Some first_instr ->
              (match this#get_unconditional_branch_block first_instr with
              | Some next_branch_block when block.id != next_branch_block.id ->
                jump_aliases <- BlockMap.add block next_branch_block jump_aliases
              | _ -> ())
            | _ -> ());

      (* Filter out jump alias blocks *)
      func.blocks <-
        List.filter
          (fun block ->
            let should_remove = BlockMap.mem block jump_aliases && func.prologue != block in
            if should_remove then this#on_removed_block block;
            not should_remove)
          func.blocks;

      (* Rewrite jumps to skip over jump alias blocks *)
      let rec resolve_jump_alias block =
        match BlockMap.find_opt block jump_aliases with
        | None -> block
        | Some alias -> resolve_jump_alias alias
      in
      this#iter_all_block_operands (fun block_op ->
          let next_block = cast_to_block block_op in
          if BlockMap.mem next_block jump_aliases then
            let resolved_alias = resolve_jump_alias next_block in
            if resolved_alias != next_block then block_op.value <- Block resolved_alias)
  end
