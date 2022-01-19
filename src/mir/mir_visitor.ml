open Basic_collections
open Mir

module IRVisitor = struct
  class t ~(program : Program.t) =
    object (this)
      val mutable visited_blocks : BlockSet.t = BlockSet.empty

      val mutable constant_vars : Value.t IMap.t = IMap.empty

      method check_visited_block block =
        if BlockSet.mem block visited_blocks then
          true
        else (
          visited_blocks <- BlockSet.add block visited_blocks;
          false
        )

      method reset_visited_blocks () = visited_blocks <- BlockSet.empty

      method run () =
        this#visit_program ();
        this#on_complete ()

      method on_complete () = ()

      method visit_program () = SMap.iter (fun _ func -> this#visit_function func) program.funcs

      method visit_function func =
        let block = func.start_block in
        this#visit_block block

      method visit_block (block : Block.t) =
        if this#check_visited_block block then
          ()
        else (
          this#visit_instructions ~block;
          this#visit_next ~block block.next
        )

      method visit_next ~block next =
        match next with
        | Halt -> ()
        | Continue continue ->
          this#visit_edge block continue;
          this#visit_block continue
        | Branch { test; continue; jump } ->
          this#visit_value test;
          this#visit_edge block continue;
          this#visit_edge block jump;
          this#visit_block continue;
          this#visit_block jump

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
        BlockMap.iter (fun _block arg_val -> this#visit_value arg_val) phi.args

      method visit_value _value = ()
    end
end
