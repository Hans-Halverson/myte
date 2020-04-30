open Ast
open Token
module Env = Parser_env.Env

module Precedence = struct
  type t =
    | Group (* Binds tightest *)
    | Unary
    | Multiplication
    | Addition
    | Comparison
    | Equality
    | LogicalAnd
    | LogicalOr
    | None (* Binds weakest *)

  let level =
    function
    | Group -> 8
    | Unary -> 7
    | Multiplication -> 6
    | Addition -> 5
    | Comparison -> 4
    | Equality -> 3
    | LogicalAnd -> 2
    | LogicalOr -> 1
    | None -> 0

  let is_tighter p1 p2 =
    level p1 > level p2
end

let mark_loc env =
  let start_loc = Env.loc env in
  (fun env -> Loc.between start_loc (Env.prev_loc env))

let rec parse_file file = parse (Parser_env.from_file file)

and parse_string str = parse (Parser_env.from_string str)

and parse env =
  let rec helper stmts =
    match Env.token env with
    | T_EOF -> List.rev stmts
    | _ ->
      let stmt = parse_statement env in
      helper (stmt :: stmts)
  in
  let (loc, statements, errors) =
    try
      let statements = helper [] in
      let loc = Env.loc env in
      (loc, statements, Env.errors env)
    with
      Parse_error.Fatal (loc, err) ->
      (loc, [], [(loc, err)])
  in
  let loc = { loc with Loc.start = Loc.first_pos } in
  ({ Program.loc; statements; t = () }, errors)

and parse_statement env =
  match Env.token env with
  | _ -> parse_expression_statement env

and parse_expression_statement env =
  let marker = mark_loc env in
  let expr = parse_expression env in
  Env.expect env T_SEMICOLON; 
  let loc = marker env in
  Statement.Expression (loc, expr)

and parse_expression ?(precedence = Precedence.None) env =
  let marker = mark_loc env in
  let expr = parse_expression_prefix env in
  let rec infix expr =
    let expr' = parse_expression_infix ~precedence env expr marker in
    if expr == expr' then
      expr
    else
      infix expr'
  in
  infix expr

and parse_expression_prefix env =
  let open Expression in
  match Env.token env with
  | T_LEFT_PAREN -> parse_group_expression env
  | T_PLUS
  | T_MINUS
  | T_LOGICAL_NOT -> parse_unary_expression env
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
  | token -> Parse_error.fatal (Env.loc env, (UnexpectedToken { actual = token; expected = None })) 

and parse_expression_infix ~precedence env left marker =
  match Env.token env with
  | T_PLUS
  | T_MINUS when Precedence.(is_tighter Addition precedence) -> parse_binary_operation env left marker
  | T_MULTIPLY
  | T_DIVIDE when Precedence.(is_tighter Multiplication precedence) -> parse_binary_operation env left marker
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LESS_THAN_OR_EQUAL
  | T_GREATER_THAN_OR_EQUAL when Precedence.(is_tighter Comparison precedence) -> parse_binary_operation env left marker
  | T_EQUALS
  | T_NOT_EQUALS when Precedence.(is_tighter Equality precedence) -> parse_binary_operation env left marker
  | T_LOGICAL_AND when Precedence.(is_tighter LogicalAnd precedence) -> parse_logical_expression env left marker
  | T_LOGICAL_OR when Precedence.(is_tighter LogicalOr precedence) -> parse_logical_expression env left marker
  | _ -> left

and parse_group_expression env =
  Env.expect env T_LEFT_PAREN;
  let expr = parse_expression env in
  Env.expect env T_RIGHT_PAREN;
  expr

and parse_unary_expression env =
  let open Expression.UnaryOperation in
  let marker = mark_loc env in
  let op =
    match Env.token env with
    | T_PLUS -> Plus
    | T_MINUS -> Minus
    | T_LOGICAL_NOT -> LogicalNot
    | _ -> failwith "Invalid prefix operator"
  in
  Env.advance env;
  let operand = parse_expression ~precedence:Unary env in
  let loc = marker env in
  Expression.UnaryOperation { loc; operand; op; t = () }

and parse_binary_operation env left marker =
  let open Expression.BinaryOperation in
  let (op, precedence) =
    match Env.token env with
    | T_PLUS -> (Add, Precedence.Addition)
    | T_MINUS -> (Subtract, Precedence.Addition)
    | T_MULTIPLY -> (Multiply, Precedence.Multiplication)
    | T_DIVIDE -> (Divide, Precedence.Multiplication)
    | T_EQUALS -> (Equal, Precedence.Equality)
    | T_NOT_EQUALS -> (NotEqual, Precedence.Equality)
    | T_LESS_THAN -> (LessThan, Precedence.Comparison)
    | T_GREATER_THAN -> (GreaterThan, Precedence.Comparison)
    | T_LESS_THAN_OR_EQUAL -> (LessThanOrEqual, Precedence.Comparison)
    | T_GREATER_THAN_OR_EQUAL -> (GreaterThanOrEqual, Precedence.Comparison)
    | _ -> failwith "Invalid binary operator"
  in
  Env.advance env;
  let right = parse_expression ~precedence env in
  let loc = marker env in
  Expression.BinaryOperation { loc; left; right; op; t = () }

and parse_logical_expression env left marker =
  let open Expression in
  match Env.token env with
  | T_LOGICAL_AND ->
    Env.advance env;
    let right = parse_expression ~precedence:LogicalAnd env in
    let loc = marker env in
    LogicalAnd { LogicalAnd.loc; left; right; t = () }
  | T_LOGICAL_OR ->
    Env.advance env;
    let right = parse_expression ~precedence:LogicalOr env in
    let loc = marker env in
    LogicalOr { LogicalOr.loc; left; right; t = () } 
  | _ -> failwith "Invalid logical operator"

and parse_identifier env =
  match Env.token env with
  | T_IDENTIFIER name ->
    let loc = Env.loc env in
    Env.advance env;
    { Identifier.loc; name; t = () }
  | _ -> assert false
