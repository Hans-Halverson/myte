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
          List.iter (this#visit_phi_node ~block) block.phis;
          this#visit_instructions ~block block.instructions;
          this#visit_next ~block block.next
        )

      method visit_phi_node ~block (_type, result_var_id, args) =
        this#visit_result_variable ~block result_var_id;
        IMap.iter (fun _block_id arg_val -> this#visit_value ~block arg_val) args

      method visit_next ~block next =
        match next with
        | Halt -> ()
        | Continue id ->
          let continue_block = get_block id in
          this#visit_edge block continue_block;
          this#visit_block continue_block
        | Branch { test; continue; jump } ->
          (match test with
          | `BoolL _ -> ()
          | `BoolV var_id -> this#visit_branch_use_variable ~block var_id);
          let continue_block = get_block continue in
          let jump_block = get_block jump in
          this#visit_edge block continue_block;
          this#visit_edge block jump_block;
          this#visit_block continue_block;
          this#visit_block jump_block

      method visit_edge _b1 _b2 = ()

      method visit_instructions ~block instructions =
        List.iter (this#visit_instruction ~block) instructions

      method visit_instruction ~block (_, instr) =
        let open Instruction in
        match instr with
        | Mov (result, arg) ->
          this#visit_value ~block arg;
          this#visit_result_variable ~block result
        | Call { return; func; args } ->
          (match func with
          | Value func -> this#visit_function_value ~block func
          | Builtin _ -> ());
          List.iter (this#visit_value ~block) args;
          (match return with
          | None -> ()
          | Some (ret, _ret_ty) -> this#visit_result_variable ~block ret)
        | Ret arg_opt -> Option.iter (this#visit_value ~block) arg_opt
        | StackAlloc (result, _ty) -> this#visit_result_variable ~block result
        | Load (result, ptr) ->
          this#visit_pointer_value ~block ptr;
          this#visit_result_variable ~block result
        | Store (ptr, arg) ->
          this#visit_value ~block arg;
          this#visit_pointer_value ~block ptr
        | GetPointer { GetPointer.var_id; return_ty = _; pointer; pointer_offset; offsets } ->
          this#visit_pointer_value ~block pointer;
          Option.iter (this#visit_numeric_value ~block) pointer_offset;
          List.iter
            (fun offset ->
              match offset with
              | GetPointer.PointerIndex index -> this#visit_numeric_value ~block index
              | GetPointer.FieldIndex _ -> ())
            offsets;
          this#visit_result_variable ~block var_id
        | Unary (_, result, arg)
        | Trunc (result, arg, _)
        | SExt (result, arg, _) ->
          this#visit_numeric_value ~block arg;
          this#visit_result_variable ~block result
        | Binary (_binary_operation, result, left, right) ->
          this#visit_numeric_value ~block left;
          this#visit_numeric_value ~block right;
          this#visit_result_variable ~block result
        | Cmp (_comparison, result, left, right) ->
          this#visit_comparable_value ~block left;
          this#visit_comparable_value ~block right;
          this#visit_result_variable ~block result

      method visit_result_variable ~block:_ _var_id = ()

      method visit_instruction_use_variable ~block var_id = this#visit_use_variable ~block var_id

      method visit_branch_use_variable = this#visit_use_variable

      method visit_use_variable ~block:_ _var_id = ()

      method visit_value ~block value =
        match value with
        | `BoolL _
        | `ByteL _
        | `IntL _
        | `LongL _
        | `FunctionL _
        | `PointerL _
        | `ArrayStringL _
        | `ArrayVtableL _ ->
          ()
        | `BoolV var_id
        | `IntV var_id
        | `ByteV var_id
        | `LongV var_id
        | `FunctionV var_id
        | `PointerV (_, var_id)
        | `ArrayV (_, _, var_id) ->
          this#visit_instruction_use_variable ~block var_id

      method visit_bool_value ~block (value : Value.bool_value) =
        this#visit_value ~block (value :> Value.t)

      method visit_long_value ~block (value : Value.long_value) =
        this#visit_value ~block (value :> Value.t)

      method visit_numeric_value ~block (value : Value.numeric_value) =
        this#visit_value ~block (value :> Value.t)

      method visit_function_value ~block (value : Value.function_value) =
        this#visit_value ~block (value :> Value.t)

      method visit_pointer_value ~block (value : Value.pointer_value) =
        this#visit_value ~block (value :> Value.t)

      method visit_comparable_value ~block (value : Value.comparable_value) =
        this#visit_value ~block (value :> Value.t)
    end
end
