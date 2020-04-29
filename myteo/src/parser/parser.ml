open Ast
open Token
module Env = Parser_env.Env

let mark_loc env =
  let start_loc = Env.loc env in
  (fun env -> Loc.between start_loc (Env.prev_loc env))

let rec parse_file file = parse (Parser_env.from_file file)

and parse_string str = parse (Parser_env.from_string str)

and parse env =
  let rec helper stmts =
    match Env.peek env with
    | T_EOF -> List.rev stmts
    | _ -> helper (parse_statement env :: stmts)
  in
  let (statements, errors) =
    try
      let statements = helper [] in
      (statements, [])
    with
      Parse_error.Fatal err ->
      let fatal = (Env.loc env, err) in
      ([], [fatal])
  in
  let loc = Env.loc env in
  let loc = { loc with Loc.start = Loc.first_pos } in
  ({ Program.loc; statements; t = () }, errors)

and parse_statement env =
  match Env.peek env with
  | _ -> parse_expression_statement env

and parse_expression_statement env =
  let marker = mark_loc env in
  let expr = parse_expression env in
  Env.expect env T_SEMICOLON; 
  let loc = marker env in
  Statement.Expression (loc, expr)

and parse_expression env = parse_unary_expression env

and parse_unary_expression env =
  let open Expression.UnaryOperation in
  let maybe_op env =
    match Env.peek env with
    | T_PLUS -> Some Plus
    | T_MINUS -> Some Minus
    | T_LOGICAL_NOT -> Some LogicalNot
    | _ -> None
  in
  let marker = mark_loc env in
  match maybe_op env with
  | Some op ->
    Env.advance env;
    let operand = parse_expression env in
    let loc = marker env in
    Expression.UnaryOperation { loc; operand; op; t = () }
  | None -> parse_binary_operation env

and parse_binary_operation env =
  let open Expression.BinaryOperation in
  let maybe_op env =
    match Env.peek env with
    | T_PLUS -> Some Add
    | T_MINUS -> Some Subtract
    | T_MULTIPLY -> Some Multiply
    | T_DIVIDE -> Some Divide
    | _ -> None
  in
  let marker = mark_loc env in
  let left = parse_logical_expression env in
  match maybe_op env with
  | Some op ->
    Env.advance env;
    let right = parse_logical_expression env in
    let loc = marker env in
    Expression.BinaryOperation { loc; left; right; op; t = () }
  | None -> left

and parse_logical_expression env =
  let open Expression in
  let marker = mark_loc env in
  let left = parse_terminal_expression env in
  match Env.peek env with
  | T_LOGICAL_AND ->
    Env.advance env;
    let right = parse_terminal_expression env in
    let loc = marker env in
    LogicalAnd { LogicalAnd.loc; left; right; t = () }
  | T_LOGICAL_OR ->
    Env.advance env;
    let right = parse_terminal_expression env in
    let loc = marker env in
    LogicalOr { LogicalOr.loc; left; right; t = () } 
  | _ -> left

and parse_terminal_expression env =
  let open Expression in
  match Env.peek env with
  | T_IDENTIFIER _ -> Identifier (parse_identifier env)
  | T_INT_LITERAL (value, raw) ->
    let loc = Env.loc env in
    Env.advance env;
    IntLiteral { IntLiteral.loc; raw; value; t = () }
  | T_STRING_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    StringLiteral { StringLiteral.loc; value; t = () }
  | T_BOOL_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    BoolLiteral { BoolLiteral.loc; value; t = () }
  | token -> Parse_error.(fatal (UnexpectedToken { actual = token; expected = None }))

and parse_identifier env =
  match Env.peek env with
  | T_IDENTIFIER name ->
    let loc = Env.loc env in
    Env.advance env;
    { Identifier.loc; name; t = () }
  | _ -> assert false
