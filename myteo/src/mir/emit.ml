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
      | _ -> ())
    mod_.toplevels

and emit_toplevel_variable_declaration ~pcx ~ecx decl =
  let { Statement.VariableDeclaration.loc; pattern; init; _ } = decl in
  let init_var = emit_expression ~pcx ~ecx init in
  let { Identifier.name; _ } = Ast_utils.id_of_pattern pattern in
  let block = Ecx.finish_block_halt ~ecx ~debug:true name in
  Ecx.add_block ~ecx block;
  Ecx.add_global ~ecx loc init_var block.Block.id

and emit_expression ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  let var_id = mk_var_id () in
  match expr with
  | Unit { loc } ->
    Ecx.emit ~ecx loc (LoadUnit var_id);
    var_id
  | IntLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (LoadInt (var_id, value));
    var_id
  | StringLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (LoadString (var_id, value));
    var_id
  | BoolLiteral { loc; value; _ } ->
    Ecx.emit ~ecx loc (LoadBool (var_id, value));
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
  | _ ->
    prerr_endline (Ast_pp.pp (Ast_pp.node_of_expression expr));
    failwith "Expression has not yet been converted to IR"
