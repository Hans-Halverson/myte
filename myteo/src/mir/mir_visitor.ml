open Basic_collections
open Mir

module IRVisitor = struct
  class t ~program =
    object (this)
      val mutable visited_blocks : ISet.t = ISet.empty

      val mutable constant_vars : var_id Instruction.Value.t IMap.t = IMap.empty

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

      method visit_program () =
        SMap.iter (fun _ global -> this#visit_global global) program.globals;
        SMap.iter (fun _ func -> this#visit_function func) program.funcs

      method visit_global global =
        let block = get_block global.init_start_block in
        this#visit_block block

      method visit_function func =
        let block = get_block func.body_start_block in
        this#visit_block block

      method visit_block (block : var_id Block.t) =
        if this#check_visited_block block.id then
          ()
        else (
          List.iter (this#visit_phi_node ~block) block.phis;
          this#visit_instructions ~block block.instructions;
          this#visit_next ~block block.next
        )

      method visit_phi_node ~block:_ _phi = ()

      method visit_next ~block next =
        match next with
        | Halt -> ()
        | Continue id ->
          let continue_block = get_block id in
          this#visit_edge block continue_block;
          this#visit_block continue_block
        | Branch { test; continue; jump } ->
          (match test with
          | Lit _ -> ()
          | Var var_id -> this#visit_branch_use_variable ~block var_id);
          let continue_block = get_block continue in
          let jump_block = get_block jump in
          this#visit_edge block continue_block;
          this#visit_edge block jump_block;
          this#visit_block continue_block;
          this#visit_block jump_block

      method visit_edge _b1 _b2 = ()

      method visit_instructions ~block instructions =
        List.iter (this#visit_instruction ~block) instructions

      method visit_instruction ~block ((_, instr) as instruction) =
        match instr with
        | Mov (result, arg) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_value ~block ~instruction arg
        | Call (ret, func, args) ->
          this#visit_result_variable ~block ~instruction ret;
          this#visit_function_value ~block ~instruction func;
          List.iter (this#visit_value ~block ~instruction) args
        | Ret arg_opt -> Option.iter (this#visit_value ~block ~instruction) arg_opt
        | LoadGlobal (result, _name) -> this#visit_result_variable ~block ~instruction result
        | StoreGlobal (_name, arg) -> this#visit_value ~block ~instruction arg
        | LogNot (result, arg) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_bool_value ~block ~instruction arg
        | LogAnd (result, left, right)
        | LogOr (result, left, right) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_bool_value ~block ~instruction left;
          this#visit_bool_value ~block ~instruction right
        | Neg (result, arg) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_numeric_value ~block ~instruction arg
        | Add (result, left, right)
        | Sub (result, left, right)
        | Mul (result, left, right)
        | Div (result, left, right)
        | Eq (result, left, right)
        | Neq (result, left, right)
        | Lt (result, left, right)
        | LtEq (result, left, right)
        | Gt (result, left, right)
        | GtEq (result, left, right) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_numeric_value ~block ~instruction left;
          this#visit_numeric_value ~block ~instruction right

      method visit_result_variable ~block:_ ~instruction:_ _var_id = ()

      method visit_instruction_use_variable ~block ~instruction:_ var_id =
        this#visit_use_variable ~block var_id

      method visit_branch_use_variable = this#visit_use_variable

      method visit_use_variable ~block:_ _var_id = ()

      method visit_value ~block ~instruction value =
        match value with
        | Unit value -> this#visit_unit_value ~block ~instruction value
        | Bool value -> this#visit_bool_value ~block ~instruction value
        | String value -> this#visit_string_value ~block ~instruction value
        | Numeric value -> this#visit_numeric_value ~block ~instruction value
        | Function value -> this#visit_function_value ~block ~instruction value

      method visit_unit_value ~block ~instruction value =
        match value with
        | Lit -> ()
        | Var var_id -> this#visit_instruction_use_variable ~block ~instruction var_id

      method visit_bool_value ~block ~instruction value =
        match value with
        | Lit _ -> ()
        | Var var_id -> this#visit_instruction_use_variable ~block ~instruction var_id

      method visit_string_value ~block ~instruction value =
        match value with
        | Lit _ -> ()
        | Var var_id -> this#visit_instruction_use_variable ~block ~instruction var_id

      method visit_numeric_value ~block ~instruction value =
        match value with
        | IntLit _ -> ()
        | IntVar var_id -> this#visit_instruction_use_variable ~block ~instruction var_id

      method visit_function_value ~block ~instruction value =
        match value with
        | Lit _ -> ()
        | Var var_id -> this#visit_instruction_use_variable ~block ~instruction var_id
    end
end
