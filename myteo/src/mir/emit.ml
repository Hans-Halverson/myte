open Ast
open Mir
module Ecx = Emit_context

let rec emit_control_flow_ir (pcx : Lex_analyze.program_context) : cf_program =
  let ecx = Emit_context.mk () in
  List.iter (emit_module ~pcx ~ecx) pcx.modules;
  {
    Program.main_id = ecx.main_id;
    blocks = Ecx.builders_to_blocks ecx.blocks;
    globals = ecx.globals;
    funcs = ecx.funcs;
  }

and emit_module ~pcx ~ecx (_, mod_) =
  let open Module in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> emit_toplevel_variable_declaration ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels;
  List.iter
    (fun toplevel ->
      match toplevel with
      | FunctionDeclaration decl -> emit_toplevel_function_declaration ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels

and value_type_of_decl_loc ~pcx loc =
  let open Lex_analyze in
  let tvar_id = Bindings.get_tvar_id_from_value_decl pcx.bindings loc in
  Type_context.find_rep_type ~cx:pcx.type_ctx (Types.TVar tvar_id) |> type_to_value_type

and emit_toplevel_variable_declaration ~pcx ~ecx decl =
  let { Statement.VariableDeclaration.pattern; init; _ } = decl in
  let { Identifier.loc; name } = Ast_utils.id_of_pattern pattern in
  (* Find value type of variable *)
  let ty = value_type_of_decl_loc ~pcx loc in
  (* Build IR for variable init *)
  Ecx.start_block_sequence ~ecx;
  ignore (Ecx.start_new_block ~ecx);
  let init_val = emit_expression ~pcx ~ecx init in
  Ecx.emit ~ecx loc (StoreGlobal (loc, init_val));
  Ecx.finish_block_halt ~ecx;
  let block_ids = Ecx.get_block_sequence ~ecx in
  Ecx.add_global ~ecx { Global.loc; name; ty; init = block_ids }

and emit_toplevel_function_declaration ~pcx ~ecx decl =
  let open Ast.Function in
  let { name = { Identifier.loc; name }; params; body; _ } = decl in
  (* Build IR for function body *)
  let param_locs_and_ids =
    List.map (fun { Param.name = { Identifier.loc; _ }; _ } -> (loc, mk_var_id ())) params
  in
  let block_ids =
    Ecx.start_block_sequence ~ecx;
    let block_id = Ecx.start_new_block ~ecx in
    if loc = pcx.main_loc then ecx.main_id <- block_id;
    (match body with
    | Block { Statement.Block.statements; _ } -> List.iter (emit_statement ~pcx ~ecx) statements
    | Expression expr ->
      let ret_val = emit_expression ~pcx ~ecx expr in
      let expr_loc = Ast_utils.expression_loc expr in
      Ecx.emit ~ecx expr_loc (Ret (Some ret_val)));
    Ecx.finish_block_halt ~ecx;
    Ecx.get_block_sequence ~ecx
  in
  (* Find value type of function *)
  let func_tvar_id = Bindings.get_tvar_id_from_value_decl pcx.bindings loc in
  let (param_tys, return_ty) =
    match Type_context.find_rep_type ~cx:pcx.type_ctx (Types.TVar func_tvar_id) with
    | Types.Function { params; return } ->
      (List.map type_to_value_type params, type_to_value_type return)
    | _ -> failwith "Function must resolve to function type"
  in
  let params = List.map2 (fun (loc, var_id) ty -> (loc, var_id, ty)) param_locs_and_ids param_tys in
  Ecx.add_function ~ecx { Function.loc; name; params; return_ty; body = block_ids }

and emit_expression ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  match expr with
  | Unit _ -> Unit Lit
  | IntLiteral { value; _ } -> Numeric (IntLit value)
  | StringLiteral { value; _ } -> String (Lit value)
  | BoolLiteral { value; _ } -> Bool (Lit value)
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~pcx ~ecx operand
  | UnaryOperation { op = Minus; loc; operand } ->
    let var_id = mk_cf_var_id () in
    let operand_val = emit_numeric_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (Neg (var_id, operand_val));
    var_value_of_type var_id Int
  | UnaryOperation { op = LogicalNot; loc; operand } ->
    let var_id = mk_cf_var_id () in
    let operand_val = emit_bool_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (LogNot (var_id, operand_val));
    var_value_of_type var_id Bool
  | LogicalAnd { loc; left; right } ->
    (* Short circuit when lhs is false by jumping to false case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let false_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    Ecx.finish_block_branch ~ecx left_val rhs_builder.id false_builder.id;
    (* Emit right hand side when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~pcx ~ecx right in
    let right_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx loc (Mov (right_var_id, right_val));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit false literal when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx false_builder;
    let false_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx loc (Mov (false_var_id, Bool (BoolValue.Lit false)));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join cases together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_cf_var_id () in
    Ecx.emit_phi ~ecx var_id [right_var_id; false_var_id];
    var_value_of_type var_id Bool
  | LogicalOr { loc; left; right } ->
    (* Short circuit when lhs is true by jumping to true case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let true_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    Ecx.finish_block_branch ~ecx left_val rhs_builder.id true_builder.id;
    (* Emit right hand side when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~pcx ~ecx right in
    let right_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx loc (Mov (right_var_id, right_val));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit true literal when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx true_builder;
    let true_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx loc (Mov (true_var_id, Bool (BoolValue.Lit true)));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join cases together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_cf_var_id () in
    Ecx.emit_phi ~ecx var_id [right_var_id; true_var_id];
    var_value_of_type var_id Bool
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let var_id = mk_cf_var_id () in
    let left_val = emit_numeric_expression ~pcx ~ecx left in
    let right_val = emit_numeric_expression ~pcx ~ecx right in
    let (instr, ty) =
      match op with
      | Add -> (Instruction.Add (var_id, left_val, right_val), ValueType.Int)
      | Subtract -> (Sub (var_id, left_val, right_val), Int)
      | Multiply -> (Mul (var_id, left_val, right_val), Int)
      | Divide -> (Div (var_id, left_val, right_val), Int)
      | Equal -> (Eq (var_id, left_val, right_val), Bool)
      | NotEqual -> (Neq (var_id, left_val, right_val), Bool)
      | LessThan -> (Lt (var_id, left_val, right_val), Bool)
      | GreaterThan -> (Gt (var_id, left_val, right_val), Bool)
      | LessThanOrEqual -> (LtEq (var_id, left_val, right_val), Bool)
      | GreaterThanOrEqual -> (GtEq (var_id, left_val, right_val), Bool)
    in
    Ecx.emit ~ecx loc instr;
    var_value_of_type var_id ty
  | Identifier { loc; _ }
  | ScopedIdentifier { name = { Identifier.loc; _ }; _ } ->
    let decl_loc = Bindings.get_source_decl_loc_from_value_use pcx.bindings loc in
    let ty = value_type_of_decl_loc ~pcx decl_loc in
    let var =
      if Ecx.is_global_loc ~ecx decl_loc then (
        let var_id = mk_cf_var_id () in
        Ecx.emit ~ecx loc (LoadGlobal (var_id, decl_loc));
        var_id
      ) else
        mk_cf_local loc
    in
    var_value_of_type var ty
  | TypeCast { expr; _ } -> emit_expression ~pcx ~ecx expr
  | _ ->
    prerr_endline (Ast_pp.pp (Ast_pp.node_of_expression expr));
    failwith "Expression has not yet been converted to IR"

and emit_bool_expression ~pcx ~ecx expr =
  let open Instruction in
  match emit_expression ~pcx ~ecx expr with
  | Value.Bool v -> v
  | _ -> failwith "Expected bool value"

and emit_numeric_expression ~pcx ~ecx expr =
  let open Instruction in
  match emit_expression ~pcx ~ecx expr with
  | Value.Numeric v -> v
  | _ -> failwith "Expected numeric value"

and emit_statement ~pcx ~ecx stmt =
  let open Statement in
  match stmt with
  | Expression (_, expr) -> ignore (emit_expression ~pcx ~ecx expr)
  | Block { statements; _ } -> List.iter (emit_statement ~pcx ~ecx) statements
  | If { loc = _; test; conseq; altern = None } ->
    (* Branch to conseq or join blocks *)
    let test_val = emit_bool_expression ~pcx ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id join_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~pcx ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  | If { loc = _; test; conseq; altern = Some altern } ->
    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~pcx ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let altern_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id altern_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~pcx ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit altern and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    emit_statement ~pcx ~ecx altern;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  | While { loc = _; test; body } ->
    let test_builder = Ecx.mk_block_builder ~ecx in
    let body_builder = Ecx.mk_block_builder ~ecx in
    let finish_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_continue ~ecx test_builder.id;
    (* Emit test block which branches to finish or body blocks *)
    Ecx.set_block_builder ~ecx test_builder;
    let test_val = emit_bool_expression ~pcx ~ecx test in
    Ecx.finish_block_branch ~ecx test_val body_builder.id finish_builder.id;
    (* Emit body block which continues to test block *)
    Ecx.push_loop_context ~ecx finish_builder.id test_builder.id;
    Ecx.set_block_builder ~ecx body_builder;
    emit_statement ~pcx ~ecx body;
    Ecx.finish_block_continue ~ecx test_builder.id;
    Ecx.pop_loop_context ~ecx;
    (* Start join block *)
    Ecx.set_block_builder ~ecx finish_builder
  | Return { loc; arg } ->
    let arg_val = Option.map (emit_expression ~pcx ~ecx) arg in
    Ecx.emit ~ecx loc (Ret arg_val)
  | Continue _ ->
    let (_, continue_id) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx continue_id
  | Break _ ->
    let (break_id, _) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx break_id
  | Assignment { loc; pattern; expr } ->
    let { Identifier.loc = use_loc; _ } = Ast_utils.id_of_pattern pattern in
    let decl_loc = Bindings.get_source_decl_loc_from_value_use pcx.bindings use_loc in
    let expr_val = emit_expression ~pcx ~ecx expr in
    if Ecx.is_global_loc ~ecx decl_loc then
      Ecx.emit ~ecx loc (StoreGlobal (decl_loc, expr_val))
    else
      Ecx.emit ~ecx loc (Mov (mk_cf_local use_loc, expr_val))
  | VariableDeclaration { pattern; init; _ } ->
    let { Identifier.loc; _ } = Ast_utils.id_of_pattern pattern in
    let init_val = emit_expression ~pcx ~ecx init in
    Ecx.emit ~ecx loc (Mov (mk_cf_local loc, init_val))
  | FunctionDeclaration _ -> failwith "Function declaration not yet converted to IR"

and mk_cf_var_id () = Id (mk_var_id ())

and mk_cf_local loc = Local loc

and type_to_value_type ty =
  match ty with
  | Types.Unit -> ValueType.Unit
  | Types.Bool -> ValueType.Bool
  | Types.Int -> ValueType.Int
  | Types.String -> ValueType.String
  | Types.Function _ -> failwith "Functions not yet supported as value type in IR"
  | Types.TVar _ -> failwith "TVars must be resolved for all values in IR"
  | Types.Any -> failwith "Any not allowed as value in IR"
