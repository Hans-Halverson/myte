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
  | Break { Break.loc; _ }
  | Continue { Continue.loc }
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
  | Record { loc; _ }
  | Tuple { loc; _ }
  | TypeCast { loc; _ }
  | UnaryOperation { loc; _ }
  | BinaryOperation { loc; _ }
  | LogicalAnd { loc; _ }
  | LogicalOr { loc; _ }
  | Call { loc; _ }
  | IndexedAccess { loc; _ }
  | NamedAccess { loc; _ } ->
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
  | Break _
  | Continue _
  | Assignment _ ->
    ()

let id_of_pattern patt =
  let open Pattern in
  match patt with
  | Identifier id -> id

let split_toplevels toplevels =
  let open Module in
  let rec helper (vars, funcs) toplevels =
    match toplevels with
    | [] -> (List.rev vars, List.rev funcs)
    | VariableDeclaration decl :: toplevels -> helper (decl :: vars, funcs) toplevels
    | FunctionDeclaration decl :: toplevels -> helper (vars, decl :: funcs) toplevels
    | TypeDeclaration _ :: toplevels -> helper (vars, funcs) toplevels
  in
  helper ([], []) toplevels

let modules_end_loc mods =
  match List.rev mods with
  | [] -> failwith "There is always at least one module"
  | { Module.loc; _ } :: _ -> Loc.point_end loc

let name_parts_of_scoped_ident id =
  let open ScopedIdentifier in
  List.map (fun { Identifier.name; _ } -> name) id.scopes @ [id.name.name]

let string_of_name_parts name_parts =
  name_parts |> List.map (fun { Ast.Identifier.name; _ } -> name) |> String.concat "."

let string_of_scoped_ident id =
  let open ScopedIdentifier in
  string_of_name_parts (id.scopes @ [id.name])
