open Basic_collections
open Mir
open Mir_builders

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
          get_next_blocks block |> BlockSet.iter this#visit_block
        )

      method visit_instructions ~block = iter_instructions block this#visit_instruction

      method visit_instruction _instr_val instr =
        let open Instruction in
        match instr.instr with
        | Phi phi -> this#visit_phi_node instr phi
        | Call { func; args; has_return = _ } ->
          (match func with
          | Value func -> this#visit_use func
          | MirBuiltin _ -> ());
          List.iter this#visit_use args
        | Ret arg_opt -> Option.iter this#visit_use arg_opt
        | StackAlloc _type -> ()
        | Load ptr -> this#visit_use ptr
        | Store (ptr, value) ->
          this#visit_use ptr;
          this#visit_use value
        | GetPointer { GetPointer.pointer; pointer_offset; offsets } ->
          this#visit_use pointer;
          Option.iter this#visit_use pointer_offset;
          List.iter
            (fun offset ->
              match offset with
              | GetPointer.PointerIndex index -> this#visit_use index
              | GetPointer.FieldIndex _ -> ())
            offsets
        | Mov arg -> this#visit_use arg
        | Unary (_, arg)
        | Cast arg
        | Trunc arg
        | SExt arg
        | ZExt arg ->
          this#visit_use arg
        | Binary (_, left, right)
        | Cmp (_, left, right) ->
          this#visit_use left;
          this#visit_use right
        | Unreachable -> ()
        | Continue _ -> ()
        | Branch { test; _ } -> this#visit_use test

      method visit_phi_node _instr phi =
        BlockMap.iter (fun _block arg_val -> this#visit_use arg_val) phi.args

      method visit_use _use = ()
    end
end
