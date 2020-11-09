open Ast
open Basic_collections
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
  ignore (Ecx.start_block ~ecx);
  let init_val = emit_expression ~pcx ~ecx init in
  let var_id = emit_local_var_from_val ~ecx loc init_val in
  Ecx.finish_block_halt ~ecx;
  let block_ids = Ecx.get_block_sequence ~ecx in
  Ecx.add_global ~ecx { Global.loc; name; var_id; ty; init = block_ids };
  Ecx.add_variable ~ecx loc var_id

and emit_toplevel_function_declaration ~pcx ~ecx decl =
  let open Ast.Function in
  let { name = { Identifier.loc; name }; params; body; _ } = decl in
  (* Build IR for function body *)
  Ecx.enter_variable_scope ~ecx;
  let param_ids =
    List.map
      (fun { Param.name = { Identifier.loc; _ }; _ } ->
        let var_id = mk_var_id () in
        Ecx.add_variable ~ecx loc var_id;
        var_id)
      params
  in
  let block_ids =
    Ecx.start_block_sequence ~ecx;
    ignore (Ecx.start_block ~ecx);
    (match body with
    | Block { Statement.Block.statements; _ } -> List.iter (emit_statement ~pcx ~ecx) statements
    | Expression expr ->
      let ret_val = emit_expression ~pcx ~ecx expr in
      let expr_loc = Ast_utils.expression_loc expr in
      Ecx.emit ~ecx expr_loc (Ret (Some ret_val)));
    Ecx.finish_block_halt ~ecx;
    Ecx.get_block_sequence ~ecx
  in
  Ecx.exit_variable_scope ~ecx;
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

and emit_expression ~pcx ~ecx expr : Instruction.Value.t =
  let open Expression in
  let open Instruction in
  match expr with
  | Unit _ -> Unit Lit
  | IntLiteral { value; _ } -> Numeric (IntLit value)
  | StringLiteral { value; _ } -> String (Lit value)
  | BoolLiteral { value; _ } -> Bool (Lit value)
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~pcx ~ecx operand
  | UnaryOperation { op = Minus; loc; operand } ->
    let var_id = mk_var_id () in
    let operand_val = emit_numeric_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (Neg (var_id, operand_val));
    var_value_of_type var_id Int
  | UnaryOperation { op = LogicalNot; loc; operand } ->
    let var_id = mk_var_id () in
    let operand_val = emit_bool_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (LogNot (var_id, operand_val));
    var_value_of_type var_id Bool
  | LogicalAnd { loc; left; right } ->
    let var_id = mk_var_id () in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    let right_val = emit_bool_expression ~pcx ~ecx right in
    Ecx.emit ~ecx loc (LogAnd (var_id, left_val, right_val));
    var_value_of_type var_id Bool
  | LogicalOr { loc; left; right } ->
    let var_id = mk_var_id () in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    let right_val = emit_bool_expression ~pcx ~ecx right in
    Ecx.emit ~ecx loc (LogOr (var_id, left_val, right_val));
    var_value_of_type var_id Bool
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let var_id = mk_var_id () in
    let left_val = emit_numeric_expression ~pcx ~ecx left in
    let right_val = emit_numeric_expression ~pcx ~ecx right in
    let mk_instr var_id left right =
      match op with
      | Add -> Instruction.Add (var_id, left, right)
      | Subtract -> Sub (var_id, left, right)
      | Multiply -> Mul (var_id, left, right)
      | Divide -> Div (var_id, left, right)
      | Equal -> Eq (var_id, left, right)
      | NotEqual -> Neq (var_id, left, right)
      | LessThan -> Lt (var_id, left, right)
      | GreaterThan -> Gt (var_id, left, right)
      | LessThanOrEqual -> LtEq (var_id, left, right)
      | GreaterThanOrEqual -> GtEq (var_id, left, right)
    in
    Ecx.emit ~ecx loc (mk_instr var_id left_val right_val);
    var_value_of_type var_id Int
  | Identifier { loc; _ }
  | ScopedIdentifier { name = { Identifier.loc; _ }; _ } ->
    let decl_loc = Bindings.get_source_decl_loc_from_value_use pcx.bindings loc in
    let var_id = Ecx.lookup_variable ~ecx decl_loc in
    let ty = value_type_of_decl_loc ~pcx decl_loc in
    var_value_of_type var_id ty
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
  | Block { statements; _ } ->
    Ecx.enter_variable_scope ~ecx;
    List.iter (emit_statement ~pcx ~ecx) statements;
    Ecx.exit_variable_scope ~ecx
  | If { loc = _; test; conseq; altern = None } ->
    (* Branch to conseq or join blocks *)
    let test_val = emit_bool_expression ~pcx ~ecx test in
    let conseq_builder = Ecx.mk_block_builder () in
    let join_builder = Ecx.mk_block_builder () in
    Ecx.finish_block
      ~ecx
      (Branch { test = test_val; continue = conseq_builder.id; jump = join_builder.id });
    (* Emit conseq and continue to join block*)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~pcx ~ecx conseq;
    Ecx.finish_block ~ecx (Continue join_builder.id);
    (* Join block creates phi nodes *)
    Ecx.set_block_builder ~ecx join_builder
  | If { loc = _; test; conseq; altern = Some altern } ->
    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~pcx ~ecx test in
    let conseq_builder = Ecx.mk_block_builder () in
    let altern_builder = Ecx.mk_block_builder () in
    let join_builder = Ecx.mk_block_builder () in
    Ecx.finish_block
      ~ecx
      (Branch { test = test_val; continue = conseq_builder.id; jump = altern_builder.id });
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~pcx ~ecx conseq;
    Ecx.finish_block ~ecx (Continue join_builder.id);
    (* Emit altern and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    emit_statement ~pcx ~ecx altern;
    Ecx.finish_block ~ecx (Continue join_builder.id);
    (* Join block creates phi nodes *)
    Ecx.set_block_builder ~ecx join_builder
  | Return { loc; arg } ->
    let arg_val = Option.map (emit_expression ~pcx ~ecx) arg in
    Ecx.emit ~ecx loc (Ret arg_val)
  | Assignment { loc; pattern; expr } ->
    let { Identifier.loc = use_loc; _ } = Ast_utils.id_of_pattern pattern in
    let decl_loc = Bindings.get_source_decl_loc_from_value_use pcx.bindings use_loc in
    let expr_val = emit_expression ~pcx ~ecx expr in
    let var_id =
      match (var_id_of_value_opt expr_val, LocMap.find_opt decl_loc ecx.globals) with
      | (Some _, Some { Global.var_id; _ }) ->
        Ecx.update_last_instruction_variable ~ecx var_id;
        var_id
      | (Some expr_var_id, None) -> expr_var_id
      | (None, global_opt) ->
        let var_id =
          match global_opt with
          | None -> mk_var_id ()
          | Some { Global.var_id; _ } -> var_id
        in
        Ecx.emit ~ecx loc (Mov (var_id, expr_val));
        var_id
    in
    Ecx.update_variable ~ecx decl_loc var_id
  | VariableDeclaration { pattern; init; _ } ->
    let { Identifier.loc; _ } = Ast_utils.id_of_pattern pattern in
    let init_val = emit_expression ~pcx ~ecx init in
    let var_id = emit_local_var_from_val ~ecx loc init_val in
    Ecx.add_variable ~ecx loc var_id
  | FunctionDeclaration _ -> failwith "Function declaration not yet converted to IR"

and emit_local_var_from_val ~ecx loc expr_val =
  match var_id_of_value_opt expr_val with
  | Some var_id when not (Ecx.is_global_id ~ecx var_id) ->
    (* Locals can reuse the existing local var *)
    var_id
  | _ ->
    (* Otherwise add an instruction moving expr to local var *)
    let var_id = mk_var_id () in
    Ecx.emit ~ecx loc (Mov (var_id, expr_val));
    var_id

and type_to_value_type ty =
  match ty with
  | Types.Unit -> ValueType.Unit
  | Types.Bool -> ValueType.Bool
  | Types.Int -> ValueType.Int
  | Types.String -> ValueType.String
  | Types.Function _ -> failwith "Functions not yet supported as value type in IR"
  | Types.TVar _ -> failwith "TVars must be resolved for all values in IR"
  | Types.Any -> failwith "Any not allowed as value in IR"
