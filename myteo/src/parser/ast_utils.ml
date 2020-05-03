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
