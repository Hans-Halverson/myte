open Ast
open Basic_collections
open Mir
module Ecx = Emit_context

let rec emit_control_flow_ir (pcx : Lex_analyze.program_context) : cf_program =
  let ecx = Emit_context.mk () in
  List.iter
    (fun mod_ ->
      let open Ast.Module in
      let name = Ast_utils.string_of_scoped_ident (snd mod_).module_.name in
      Ecx.start_module ~ecx name;
      emit_module ~pcx ~ecx mod_;
      Ecx.end_module ~ecx)
    pcx.modules;
  {
    Program.main_id = ecx.main_id;
    blocks = Ecx.builders_to_blocks ecx.blocks;
    globals = ecx.globals;
    funcs = ecx.funcs;
    modules = ecx.modules;
  }

and emit_module ~pcx ~ecx (_, mod_) =
  let open Ast.Module in
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

and emit_toplevel_variable_declaration ~pcx ~ecx decl =
  let { Statement.VariableDeclaration.pattern; init; _ } = decl in
  (* TODO: Emit MIR for arbitrary patterns *)
  let { Identifier.loc; name } = List.hd (Ast_utils.ids_of_pattern pattern) in
  let name = Printf.sprintf "%s.%s" (Ecx.get_module_builder ~ecx).name name in
  (* Find value type of variable *)
  let ty = value_type_of_decl_loc ~pcx loc in
  (* Build IR for variable init *)
  Ecx.start_block_sequence ~ecx (GlobalInit name);
  let init_start_block = Ecx.start_new_block ~ecx in
  let init_val = emit_expression ~pcx ~ecx init in
  Ecx.emit ~ecx (StoreGlobal (name, init_val));
  Ecx.finish_block_halt ~ecx;
  Ecx.add_global ~ecx { Global.loc; name; ty; init_start_block; init_val }

and emit_toplevel_function_declaration ~pcx ~ecx decl =
  let open Ast.Function in
  let { name = { Identifier.loc; name }; params; body; _ } = decl in
  let name = Printf.sprintf "%s.%s" (Ecx.get_module_builder ~ecx).name name in
  (* Build IR for function body *)
  let param_locs_and_ids =
    List.map (fun { Param.name = { Identifier.loc; _ }; _ } -> (loc, mk_var_id ())) params
  in
  let body_start_block =
    Ecx.start_block_sequence ~ecx (FunctionBody name);
    let body_start_block = Ecx.start_new_block ~ecx in
    if loc = pcx.main_loc then ecx.main_id <- body_start_block;
    (match body with
    | Block { Statement.Block.statements; _ } ->
      List.iter (emit_statement ~pcx ~ecx) statements;
      (* Add an implicit return if the last instruction is not a return *)
      (match ecx.current_block_builder with
      | Some { Ecx.BlockBuilder.instructions = (_, Ret _) :: _; _ } -> ()
      | _ -> Ecx.emit ~ecx (Ret None))
    | Expression expr ->
      let ret_val = emit_expression ~pcx ~ecx expr in
      Ecx.emit ~ecx (Ret (Some ret_val)));
    Ecx.finish_block_halt ~ecx;
    body_start_block
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
  Ecx.add_function ~ecx { Function.loc; name; params; return_ty; body_start_block }

and emit_expression ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  match expr with
  | Unit _ -> Unit Lit
  | IntLiteral { loc; raw; base } ->
    let value = Integers.int64_of_string_opt raw base |> Option.get in
    let ty = value_type_of_loc ~pcx loc in
    (match ty with
    | ValueType.Byte -> Numeric (ByteLit (Int64.to_int value))
    | ValueType.Int -> Numeric (IntLit (Int64.to_int32 value))
    | ValueType.Long -> Numeric (LongLit value)
    | _ -> failwith "Int literal must have integer type")
  | StringLiteral { value; _ } -> String (Lit value)
  | BoolLiteral { value; _ } -> Bool (Lit value)
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~pcx ~ecx operand
  | UnaryOperation { loc; op = Minus; operand } ->
    let var_id = mk_cf_var_id () in
    let operand_val = emit_numeric_expression ~pcx ~ecx operand in
    let ty = value_type_of_loc ~pcx loc in
    Ecx.emit ~ecx (Neg (var_id, operand_val));
    var_value_of_type var_id ty
  | UnaryOperation { op = LogicalNot; loc = _; operand } ->
    let var_id = mk_cf_var_id () in
    let operand_val = emit_bool_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx (LogNot (var_id, operand_val));
    var_value_of_type var_id Bool
  | LogicalAnd { loc = _; left; right } ->
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
    Ecx.emit ~ecx (Mov (right_var_id, right_val));
    let rhs_end_block_id = Ecx.get_block_builder_id_throws ~ecx in
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit false literal when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx false_builder;
    let false_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (false_var_id, Bool (BoolValue.Lit false)));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join cases together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_cf_var_id () in
    Ecx.emit_phi
      ~ecx
      ValueType.Bool
      var_id
      (IMap.add rhs_end_block_id right_var_id (IMap.singleton false_builder.id false_var_id));
    var_value_of_type var_id Bool
  | LogicalOr { loc = _; left; right } ->
    (* Short circuit when lhs is true by jumping to true case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let true_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    Ecx.finish_block_branch ~ecx left_val true_builder.id rhs_builder.id;
    (* Emit right hand side when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~pcx ~ecx right in
    let right_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (right_var_id, right_val));
    let rhs_end_block_id = Ecx.get_block_builder_id_throws ~ecx in
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit true literal when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx true_builder;
    let true_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (true_var_id, Bool (BoolValue.Lit true)));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join cases together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_cf_var_id () in
    Ecx.emit_phi
      ~ecx
      ValueType.Bool
      var_id
      (IMap.add rhs_end_block_id right_var_id (IMap.singleton true_builder.id true_var_id));
    var_value_of_type var_id Bool
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let var_id = mk_cf_var_id () in
    let left_val = emit_numeric_expression ~pcx ~ecx left in
    let right_val = emit_numeric_expression ~pcx ~ecx right in
    let ty = value_type_of_loc ~pcx loc in
    let (instr, ty) =
      match op with
      | Add -> (Instruction.Add (var_id, left_val, right_val), ty)
      | Subtract -> (Sub (var_id, left_val, right_val), ty)
      | Multiply -> (Mul (var_id, left_val, right_val), ty)
      | Divide -> (Div (var_id, left_val, right_val), ty)
      | Equal -> (Eq (var_id, left_val, right_val), Bool)
      | NotEqual -> (Neq (var_id, left_val, right_val), Bool)
      | LessThan -> (Lt (var_id, left_val, right_val), Bool)
      | GreaterThan -> (Gt (var_id, left_val, right_val), Bool)
      | LessThanOrEqual -> (LtEq (var_id, left_val, right_val), Bool)
      | GreaterThanOrEqual -> (GtEq (var_id, left_val, right_val), Bool)
    in
    Ecx.emit ~ecx instr;
    var_value_of_type var_id ty
  | Identifier { loc; _ }
  | ScopedIdentifier { name = { Identifier.loc; _ }; _ } ->
    let binding = Bindings.get_source_value_binding pcx.Lex_analyze.bindings loc in
    let decl_loc = fst binding.declaration in
    (match snd binding.declaration with
    | ImportedModule _ -> failwith "Modules cannot appear in a value position"
    | CtorDecl
    | ImportedCtorDecl _ ->
      failwith "Constructors cannot appear in a value position"
    (* Create function literal for functions *)
    | FunDecl
    | ImportedFunDecl _ ->
      Function (Lit (mk_binding_name binding))
    (* Variables may be either globals or locals *)
    | VarDecl _
    | ImportedVarDecl _
    | FunParam ->
      let var =
        if Bindings.is_global_decl pcx.bindings decl_loc then (
          let var_id = mk_cf_var_id () in
          Ecx.emit ~ecx (LoadGlobal (var_id, mk_binding_name binding));
          var_id
        ) else
          mk_cf_local loc
      in
      var_value_of_type var (value_type_of_decl_loc ~pcx decl_loc))
  | TypeCast { expr; _ } -> emit_expression ~pcx ~ecx expr
  | Call { loc; func; args } ->
    let var_id = mk_cf_var_id () in
    let func_val = emit_function_expression ~pcx ~ecx func in
    let arg_vals = List.map (emit_expression ~pcx ~ecx) args in
    let ret_ty = value_type_of_loc ~pcx loc in
    Ecx.emit ~ecx (Call (var_id, ret_ty, func_val, arg_vals));
    var_value_of_type var_id ret_ty
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

and emit_function_expression ~pcx ~ecx expr =
  let open Instruction in
  match emit_expression ~pcx ~ecx expr with
  | Value.Function v -> v
  | _ -> failwith "Expected function value"

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
  | Return { loc = _; arg } ->
    let arg_val = Option.map (emit_expression ~pcx ~ecx) arg in
    Ecx.emit ~ecx (Ret arg_val)
  | Continue _ ->
    let (_, continue_id) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx continue_id
  | Break _ ->
    let (break_id, _) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx break_id
  | Assignment { loc = _; pattern; expr } ->
    (* TODO: Emit MIR for arbitrary patterns *)
    let { Identifier.loc = use_loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
    let binding = Bindings.get_source_value_binding pcx.bindings use_loc in
    let decl_loc = fst binding.declaration in
    let expr_val = emit_expression ~pcx ~ecx expr in
    if Bindings.is_global_decl pcx.bindings decl_loc then
      Ecx.emit ~ecx (StoreGlobal (mk_binding_name binding, expr_val))
    else
      Ecx.emit ~ecx (Mov (mk_cf_local use_loc, expr_val))
  | Match _ -> failwith "TODO: Convert match statements to IR"
  | VariableDeclaration { pattern; init; _ } ->
    (* TODO: Emit MIR for arbitrary patterns *)
    let { Identifier.loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
    let init_val = emit_expression ~pcx ~ecx init in
    Ecx.emit ~ecx (Mov (mk_cf_local loc, init_val))
  | FunctionDeclaration _ -> failwith "Function declaration not yet converted to IR"

and mk_cf_var_id () = Id (mk_var_id ())

and mk_cf_local loc = Local loc

and mk_binding_name binding = String.concat "." (binding.module_ @ [binding.name])

and type_to_value_type ty =
  match ty with
  | Types.Unit -> ValueType.Unit
  | Types.Bool -> ValueType.Bool
  | Types.Byte -> ValueType.Byte
  | Types.Int -> ValueType.Int
  | Types.Long -> ValueType.Long
  | Types.IntLiteral { resolved; _ } -> type_to_value_type (Option.get resolved)
  | Types.String -> ValueType.String
  | Types.Tuple _ -> failwith "TODO: Implement MIR emission for tuple types"
  | Types.Function _ -> ValueType.Function
  | Types.ADT _ -> failwith "TODO: Implement MIR emission for ADTs"
  | Types.TVar _ -> failwith "TVars must be resolved for all values in IR"
  | Types.Any -> failwith "Any not allowed as value in IR"

and value_type_of_loc ~pcx loc =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:pcx.type_ctx loc in
  let ty = Type_context.find_rep_type ~cx:pcx.type_ctx (Types.TVar tvar_id) in
  type_to_value_type ty

and value_type_of_decl_loc ~pcx loc =
  let open Lex_analyze in
  let tvar_id = Bindings.get_tvar_id_from_value_decl pcx.bindings loc in
  let ty = Type_context.find_rep_type ~cx:pcx.type_ctx (Types.TVar tvar_id) in
  type_to_value_type ty
