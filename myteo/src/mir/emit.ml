open Ast
open Mir
module Ecx = Emit_context

let rec emit_program (pcx : Lex_analyze.program_context) =
  let ecx = Emit_context.mk () in
  List.iter (emit_module ~pcx ~ecx) pcx.modules;
  { Program.main_id = ecx.main_id; blocks = ecx.blocks; globals = ecx.globals; funcs = ecx.funcs }

and emit_module ~pcx ~ecx (_, mod_) =
  let open Module in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> emit_toplevel_variable_declaration ~pcx ~ecx decl
      | FunctionDeclaration decl -> emit_toplevel_function_declaration ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels

and emit_toplevel_variable_declaration ~pcx ~ecx decl =
  let { Statement.VariableDeclaration.loc; pattern; init; _ } = decl in
  let { Identifier.loc = id_loc; name } = Ast_utils.id_of_pattern pattern in
  (* Build IR for variable init *)
  Ecx.start_block_sequence ~ecx;
  ignore (emit_expression ~pcx ~ecx init);
  Ecx.finish_block_halt ~ecx;
  let block_ids = Ecx.get_block_sequence ~ecx in
  (* Find value type of variable *)
  let tvar_id = Bindings.get_tvar_id_from_value_decl pcx.bindings id_loc in
  let ty = Type_context.find_rep_type ~cx:pcx.type_ctx (Types.TVar tvar_id) |> type_to_value_type in
  Ecx.add_global ~ecx { Global.loc; name; ty; init = block_ids }

and emit_toplevel_function_declaration ~pcx ~ecx decl =
  let open Ast.Function in
  let { name = { Identifier.loc; name }; params; body; _ } = decl in
  (* Build IR for function body *)
  let param_ids = List.map (fun _ -> mk_var_id ()) params in
  let block_ids =
    Ecx.start_block_sequence ~ecx;
    match body with
    | Block { Statement.Block.statements; _ } ->
      List.iter (emit_statement ~pcx ~ecx) statements;
      Ecx.finish_block_halt ~ecx;
      Ecx.get_block_sequence ~ecx
    | Expression expr ->
      let ret_var = emit_expression ~pcx ~ecx expr in
      let expr_loc = Ast_utils.expression_loc expr in
      Ecx.emit ~ecx expr_loc (Ret (Some ret_var));
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
  let params = List.combine param_ids param_tys in
  Ecx.add_function ~ecx { Function.loc; name; params; return_ty; body = block_ids }

and emit_expression ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  let var_id = mk_var_id () in
  match expr with
  | Unit { loc } ->
    Ecx.emit ~ecx loc (Lit (var_id, LitValue.Unit));
    var_id
  | IntLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (Lit (var_id, LitValue.Int value));
    var_id
  | StringLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (Lit (var_id, LitValue.String value));
    var_id
  | BoolLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (Lit (var_id, LitValue.Bool value));
    var_id
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~pcx ~ecx operand
  | UnaryOperation { op = Minus; loc; operand } ->
    let operand_var_id = emit_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (Neg (NumericType.Int, var_id, operand_var_id));
    var_id
  | UnaryOperation { op = LogicalNot; loc; operand } ->
    let operand_var_id = emit_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (LogNot (var_id, operand_var_id));
    var_id
  | LogicalAnd { loc; left; right } ->
    let left_var_id = emit_expression ~pcx ~ecx left in
    let right_var_id = emit_expression ~pcx ~ecx right in
    Ecx.emit ~ecx loc (LogAnd (var_id, left_var_id, right_var_id));
    var_id
  | LogicalOr { loc; left; right } ->
    let left_var_id = emit_expression ~pcx ~ecx left in
    let right_var_id = emit_expression ~pcx ~ecx right in
    Ecx.emit ~ecx loc (LogOr (var_id, left_var_id, right_var_id));
    var_id
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let left_var_id = emit_expression ~pcx ~ecx left in
    let right_var_id = emit_expression ~pcx ~ecx right in
    let mk_instr id1 id2 id3 =
      match op with
      | Add -> Instruction.Add (NumericType.Int, id1, id2, id3)
      | Subtract -> Sub (NumericType.Int, id1, id2, id3)
      | Multiply -> Mul (NumericType.Int, id1, id2, id3)
      | Divide -> Div (NumericType.Int, id1, id2, id3)
      | Equal -> Eq (NumericType.Int, id1, id2, id3)
      | NotEqual -> Neq (NumericType.Int, id1, id2, id3)
      | LessThan -> Lt (NumericType.Int, id1, id2, id3)
      | GreaterThan -> Gt (NumericType.Int, id1, id2, id3)
      | LessThanOrEqual -> LtEq (NumericType.Int, id1, id2, id3)
      | GreaterThanOrEqual -> GtEq (NumericType.Int, id1, id2, id3)
    in
    Ecx.emit ~ecx loc (mk_instr var_id left_var_id right_var_id);
    var_id
  | _ ->
    prerr_endline (Ast_pp.pp (Ast_pp.node_of_expression expr));
    failwith "Expression has not yet been converted to IR"

and emit_statement ~pcx ~ecx stmt =
  let open Statement in
  match stmt with
  | Expression (_, expr) -> ignore (emit_expression ~pcx ~ecx expr)
  | Block { statements; _ } -> List.iter (emit_statement ~pcx ~ecx) statements
  | If _ -> failwith "If statement not yet converted to IR"
  | Return { loc; arg } ->
    let arg_var_id = Option.map (emit_expression ~pcx ~ecx) arg in
    Ecx.emit ~ecx loc (Ret arg_var_id)
  | VariableDeclaration { init; _ } ->
    let init_var = emit_expression ~pcx ~ecx init in
    ignore init_var
  | FunctionDeclaration _ -> failwith "Function declaration not yet converted to IR"

and type_to_value_type ty =
  match ty with
  | Types.Unit -> ValueType.Unit
  | Types.Bool -> ValueType.Bool
  | Types.Int -> ValueType.Int
  | Types.String -> ValueType.String
  | Types.Function _ -> failwith "Functions not yet supported as value type in IR"
  | Types.TVar _ -> failwith "TVars must be resolved for all values in IR"
  | Types.Any -> failwith "Any not allowed as value in IR"
