open Ast

let statement_loc stmt =
  let open Statement in
  match stmt with
  | VariableDeclaration { VariableDeclaration.loc; _ }
  | FunctionDeclaration { Function.loc; _ }
  | Expression (loc, _)
  | Block { Block.loc; _ }
  | If { If.loc; _ }
  | While { While.loc; _ }
  | Return { Return.loc; _ }
  | Assignment { Assignment.loc; _ } ->
    loc

let expression_loc expr =
  let open Expression in
  match expr with
  | Unit { loc }
  | IntLiteral { loc; _ }
  | StringLiteral { loc; _ }
  | BoolLiteral { loc; _ }
  | Identifier { loc; _ }
  | ScopedIdentifier { loc; _ }
  | TypeCast { loc; _ }
  | UnaryOperation { loc; _ }
  | BinaryOperation { loc; _ }
  | LogicalAnd { loc; _ }
  | LogicalOr { loc; _ }
  | Call { loc; _ }
  | Access { loc; _ } ->
    loc

let rec statement_visitor ~f ?(enter_functions = true) stmt =
  let open Statement in
  f stmt;
  match stmt with
  | Block { Block.statements; _ } -> List.iter (statement_visitor ~f) statements
  | If { If.conseq; altern; _ } ->
    statement_visitor ~f conseq;
    Option.iter (statement_visitor ~f) altern
  | While { While.body; _ } -> statement_visitor ~f body
  | FunctionDeclaration { Function.body = Function.Block block; _ } ->
    if enter_functions then statement_visitor ~f (Block block)
  | FunctionDeclaration { Function.body = Function.Expression _; _ }
  | VariableDeclaration _
  | Expression _
  | Return _
  | Assignment _ ->
    ()

let id_of_pattern patt =
  let open Pattern in
  match patt with
  | Identifier id -> id
