open Basic_collections
open Mir

module IRVisitor = struct
  class t ~program =
    object (this)
      val mutable visited_blocks : ISet.t = ISet.empty

      val mutable constant_vars : Value.t IMap.t = IMap.empty

      val get_block = (fun block_id -> IMap.find block_id program.Program.blocks)

      method check_visited_block block_id =
        if ISet.mem block_id visited_blocks then
          true
        else (
          visited_blocks <- ISet.add block_id visited_blocks;
          false
        )

      method reset_visited_blocks () = visited_blocks <- ISet.empty

      method run () =
        this#visit_program ();
        this#on_complete ()

      method on_complete () = ()

      method visit_program () = SMap.iter (fun _ func -> this#visit_function func) program.funcs

      method visit_function func =
        let block = get_block func.body_start_block in
        this#visit_block block

      method visit_block (block : Block.t) =
        if this#check_visited_block block.id then
          ()
        else (
          this#visit_instructions ~block;
          this#visit_next ~block block.next
        )

      method visit_next ~block next =
        match next with
        | Halt -> ()
        | Continue id ->
          let continue_block = get_block id in
          this#visit_edge block continue_block;
          this#visit_block continue_block
        | Branch { test; continue; jump } ->
          this#visit_value test;
          let continue_block = get_block continue in
          let jump_block = get_block jump in
          this#visit_edge block continue_block;
          this#visit_edge block jump_block;
          this#visit_block continue_block;
          this#visit_block jump_block

      method visit_edge _b1 _b2 = ()

      method visit_instructions ~block = iter_instructions block this#visit_instruction

      method visit_instruction instr =
        let open Instruction in
        match instr.instr with
        | Phi phi -> this#visit_phi_node instr phi
        | Call { func; args; has_return = _ } ->
          (match func with
          | Value func -> this#visit_value func
          | Builtin _ -> ());
          List.iter this#visit_value args
        | Ret arg_opt -> Option.iter this#visit_value arg_opt
        | StackAlloc _type -> ()
        | Load ptr -> this#visit_value ptr
        | Store (ptr, value) ->
          this#visit_value ptr;
          this#visit_value value
        | GetPointer { GetPointer.pointer; pointer_offset; offsets } ->
          this#visit_value pointer;
          Option.iter this#visit_value pointer_offset;
          List.iter
            (fun offset ->
              match offset with
              | GetPointer.PointerIndex index -> this#visit_value index
              | GetPointer.FieldIndex _ -> ())
            offsets
        | Mov arg -> this#visit_value arg
        | Unary (_, arg)
        | Cast arg
        | Trunc arg
        | SExt arg ->
          this#visit_value arg
        | Binary (_, left, right)
        | Cmp (_, left, right) ->
          this#visit_value left;
          this#visit_value right

      method visit_phi_node _instr phi =
        IMap.iter (fun _block_id arg_val -> this#visit_value arg_val) phi.args

      method visit_value _value = ()
    end
end
