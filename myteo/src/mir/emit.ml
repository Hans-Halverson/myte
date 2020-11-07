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
  let { Identifier.name; _ } = Ast_utils.id_of_pattern pattern in
  Ecx.start_block_sequence ~ecx (Block.GlobalLabel name);
  let init_var = emit_expression ~pcx ~ecx init in
  Ecx.finish_block_halt ~ecx;
  let block_ids = Ecx.get_block_sequence ~ecx in
  Ecx.add_global ~ecx loc init_var block_ids

and emit_toplevel_function_declaration ~pcx ~ecx decl =
  let open Ast.Function in
  let { name = { Identifier.loc; name }; params; body; _ } = decl in
  let param_ids = List.map (fun _ -> mk_var_id ()) params in
  let block_ids =
    Ecx.start_block_sequence ~ecx (Block.FuncLabel name);
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
  Ecx.add_function ~ecx { Function.loc; name; params = param_ids; body = block_ids }

and emit_expression ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  let var_id = mk_var_id () in
  match expr with
  | Unit { loc } ->
    Ecx.emit ~ecx loc (LitUnit var_id);
    var_id
  | IntLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (LitInt (var_id, value));
    var_id
  | StringLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (LitString (var_id, value));
    var_id
  | BoolLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (LitBool (var_id, value));
    var_id
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~pcx ~ecx operand
  | UnaryOperation { op = Minus; loc; operand } ->
    let operand_var_id = emit_expression ~pcx ~ecx operand in
    Ecx.emit ~ecx loc (NegInt (var_id, operand_var_id));
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
      | Add -> AddInt (id1, id2, id3)
      | Subtract -> SubInt (id1, id2, id3)
      | Multiply -> MulInt (id1, id2, id3)
      | Divide -> DivInt (id1, id2, id3)
      | Equal -> EqInt (id1, id2, id3)
      | NotEqual -> NeqInt (id1, id2, id3)
      | LessThan -> LtInt (id1, id2, id3)
      | GreaterThan -> GtInt (id1, id2, id3)
      | LessThanOrEqual -> LteqInt (id1, id2, id3)
      | GreaterThanOrEqual -> GteqInt (id1, id2, id3)
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
