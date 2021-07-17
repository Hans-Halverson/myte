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
  | Assignment { Assignment.loc; _ }
  | Match { Match.loc; _ } ->
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
  | Ternary { loc; _ }
  | Call { loc; _ }
  | IndexedAccess { loc; _ }
  | NamedAccess { loc; _ }
  | Match { loc; _ }
  | Super loc ->
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
  | Match { Match.cases; _ } ->
    List.iter
      (fun { Match.Case.right; _ } ->
        match right with
        | Match.Case.Statement stmt -> statement_visitor ~f stmt
        | _ -> ())
      cases
  | FunctionDeclaration { Function.body = Function.Expression _ | Function.Signature; _ }
  | VariableDeclaration _
  | Expression _
  | Return _
  | Break _
  | Continue _
  | Assignment _ ->
    ()

let ids_of_pattern patt =
  let rec helper patt acc =
    let open Pattern in
    match patt with
    | Wildcard _
    | Literal _ ->
      acc
    | Identifier id -> id :: acc
    | Tuple { Tuple.elements; _ } ->
      List.fold_left (fun acc element -> helper element acc) acc elements
    | Record { Record.fields; _ } ->
      List.fold_left (fun acc { Record.Field.value; _ } -> helper value acc) acc fields
  in
  List.rev (helper patt [])

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
