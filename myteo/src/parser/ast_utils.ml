open Ast

let statement_loc stmt =
  let open Statement in
  match stmt with
  | Expression (loc, _)
  | Block { Block.loc; _ }
  | If { If.loc; _ }
  | Return { Return.loc; _ }
  | VariableDeclaration { VariableDeclaration.loc; _ }
  | FunctionDeclaration { Function.loc; _ } ->
    loc

let rec statement_visitor ~f ?(enter_functions = true) stmt =
  let open Statement in
  f stmt;
  match stmt with
  | Block { Block.statements; _ } -> List.iter (statement_visitor ~f) statements
  | If { If.conseq; altern; _ } ->
    statement_visitor ~f conseq;
    Option.iter (statement_visitor ~f) altern
  | FunctionDeclaration { Function.body = Function.Block block; _ } ->
    if enter_functions then statement_visitor ~f (Block block)
  | FunctionDeclaration { Function.body = Function.Expression _; _ }
  | VariableDeclaration _
  | Expression _
  | Return _ ->
    ()
