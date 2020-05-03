open Ast
open Token
module Env = Parser_env.Env

module ExpressionPrecedence = struct
  type t =
    | (* Binds tightest *) Group
    | Call
    | Unary
    | Multiplication
    | Addition
    | Comparison
    | Equality
    | LogicalAnd
    | LogicalOr
    | (* Binds weakest *) None

  let level = function
    | Group -> 9
    | Call -> 8
    | Unary -> 7
    | Multiplication -> 6
    | Addition -> 5
    | Comparison -> 4
    | Equality -> 3
    | LogicalAnd -> 2
    | LogicalOr -> 1
    | None -> 0

  let is_tighter p1 p2 = level p1 > level p2
end

module TypePrecedence = struct
  type t =
    | (* Binds tightest *) Group
    | Function
    | (* Binds weakest *) None

  let level = function
    | Group -> 2
    | Function -> 1
    | None -> 0

  let is_tighter p1 p2 = level p1 > level p2
end

let mark_loc env =
  let start_loc = Env.loc env in
  (fun env -> Loc.between start_loc (Env.prev_loc env))

let rec parse_file file = parse (Parser_env.from_file file)

and parse_string str = parse (Parser_env.from_string str)

and parse env =
  let rec helper toplevels =
    match Env.token env with
    | T_EOF -> List.rev toplevels
    | _ ->
      let toplevel = parse_toplevel env in
      helper (toplevel :: toplevels)
  in
  let (loc, toplevels, errors) =
    try
      let toplevels = helper [] in
      let loc = Env.loc env in
      (loc, toplevels, Env.errors env)
    with Parse_error.Fatal (loc, err) -> (loc, [], [(loc, err)])
  in
  let loc = { loc with Loc.start = Loc.first_pos } in
  ({ Program.loc; toplevels; t = () }, errors)

and parse_toplevel env =
  let open Program in
  match Env.token env with
  | T_VAL
  | T_VAR ->
    VariableDeclaration (parse_variable_declaration env)
  | T_FUN -> FunctionDeclaration (parse_function env)
  | token -> Parse_error.fatal (Env.loc env, MalformedTopLevel token)

and parse_statement env =
  let open Statement in
  match Env.token env with
  | T_LEFT_BRACE -> Block (parse_block env)
  | T_IF -> parse_if env
  | T_RETURN -> parse_return env
  | T_VAL
  | T_VAR ->
    VariableDeclaration (parse_variable_declaration env)
  | T_FUN -> FunctionDeclaration (parse_function env)
  | _ -> parse_expression_statement env

and parse_expression_statement env =
  let marker = mark_loc env in
  let expr = parse_expression env in
  Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Expression (loc, expr)

and parse_expression ?(precedence = ExpressionPrecedence.None) env =
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
  | T_LOGICAL_NOT ->
    parse_unary_expression env
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
  | token -> Parse_error.fatal (Env.loc env, UnexpectedToken { actual = token; expected = None })

and parse_expression_infix ~precedence env left marker =
  match Env.token env with
  | T_LEFT_PAREN when ExpressionPrecedence.(is_tighter Call precedence) ->
    parse_call env left marker
  | T_PLUS
  | T_MINUS
    when ExpressionPrecedence.(is_tighter Addition precedence) ->
    parse_binary_operation env left marker
  | T_MULTIPLY
  | T_DIVIDE
    when ExpressionPrecedence.(is_tighter Multiplication precedence) ->
    parse_binary_operation env left marker
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LESS_THAN_OR_EQUAL
  | T_GREATER_THAN_OR_EQUAL
    when ExpressionPrecedence.(is_tighter Comparison precedence) ->
    parse_binary_operation env left marker
  | T_DOUBLE_EQUALS
  | T_NOT_EQUALS
    when ExpressionPrecedence.(is_tighter Equality precedence) ->
    parse_binary_operation env left marker
  | T_LOGICAL_AND when ExpressionPrecedence.(is_tighter LogicalAnd precedence) ->
    parse_logical_expression env left marker
  | T_LOGICAL_OR when ExpressionPrecedence.(is_tighter LogicalOr precedence) ->
    parse_logical_expression env left marker
  | _ -> left

and parse_group_expression env =
  let open Expression in
  Env.expect env T_LEFT_PAREN;
  match Env.token env with
  | T_RIGHT_PAREN ->
    let loc = Env.loc env in
    Env.advance env;
    Unit { Unit.loc; t = () }
  | _ ->
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
    | T_PLUS -> (Add, ExpressionPrecedence.Addition)
    | T_MINUS -> (Subtract, ExpressionPrecedence.Addition)
    | T_MULTIPLY -> (Multiply, ExpressionPrecedence.Multiplication)
    | T_DIVIDE -> (Divide, ExpressionPrecedence.Multiplication)
    | T_DOUBLE_EQUALS -> (Equal, ExpressionPrecedence.Equality)
    | T_NOT_EQUALS -> (NotEqual, ExpressionPrecedence.Equality)
    | T_LESS_THAN -> (LessThan, ExpressionPrecedence.Comparison)
    | T_GREATER_THAN -> (GreaterThan, ExpressionPrecedence.Comparison)
    | T_LESS_THAN_OR_EQUAL -> (LessThanOrEqual, ExpressionPrecedence.Comparison)
    | T_GREATER_THAN_OR_EQUAL -> (GreaterThanOrEqual, ExpressionPrecedence.Comparison)
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

and parse_call env left marker =
  let open Expression.Call in
  Env.expect env T_LEFT_PAREN;
  let rec args env =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let arg = parse_expression env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      arg :: args env
  in
  let args = args env in
  let loc = marker env in
  Expression.Call { loc; func = left; args; t = () }

and parse_identifier env =
  match Env.token env with
  | T_IDENTIFIER name ->
    let loc = Env.loc env in
    Env.advance env;
    { Identifier.loc; name; t = () }
  | token ->
    Parse_error.fatal
      (Env.loc env, UnexpectedToken { actual = token; expected = Some (T_IDENTIFIER "") })

and parse_pattern env =
  let open Pattern in
  match Env.token env with
  | T_IDENTIFIER _ -> Identifier (parse_identifier env)
  | token -> Parse_error.fatal (Env.loc env, MalformedPattern token)

and parse_block env =
  let open Statement in
  let marker = mark_loc env in
  Env.expect env T_LEFT_BRACE;
  let rec statements env =
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | _ ->
      let statement = parse_statement env in
      statement :: statements env
  in
  let statements = statements env in
  let loc = marker env in
  { Block.loc; statements; t = () }

and parse_if env =
  let open Statement.If in
  let marker = mark_loc env in
  Env.expect env T_IF;
  Env.expect env T_LEFT_PAREN;
  let test = parse_expression env in
  Env.expect env T_RIGHT_PAREN;
  let conseq = parse_statement env in
  let altern =
    match Env.token env with
    | T_ELSE ->
      Env.advance env;
      Some (parse_statement env)
    | _ -> None
  in
  let loc = marker env in
  Statement.If { loc; test; conseq; altern; t = () }

and parse_return env =
  let open Statement.Return in
  let marker = mark_loc env in
  Env.expect env T_RETURN;
  let arg = parse_expression env in
  Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Return { loc; arg; t = () }

and parse_variable_declaration env =
  let open Statement in
  let marker = mark_loc env in
  let kind =
    match Env.token env with
    | T_VAL -> VariableDeclaration.Immutable
    | T_VAR -> VariableDeclaration.Mutable
    | _ -> failwith "Must be called on variable declaration"
  in
  Env.advance env;
  let pattern = parse_pattern env in
  let annot =
    match Env.token env with
    | T_COLON ->
      Env.advance env;
      Some (parse_type env)
    | _ -> None
  in
  Env.expect env T_EQUALS;
  let init = parse_expression env in
  Env.expect env T_SEMICOLON;
  let loc = marker env in
  { VariableDeclaration.loc; kind; pattern; init; annot; t = () }

and parse_function env =
  let open Function in
  let marker = mark_loc env in
  Env.expect env T_FUN;
  let name = parse_identifier env in
  Env.expect env T_LEFT_PAREN;
  let rec params env =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let marker = mark_loc env in
      let name = parse_identifier env in
      Env.expect env T_COLON;
      let annot = parse_type env in
      let loc = marker env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      let param = { Param.loc; name; annot; t = () } in
      param :: params env
  in
  let params = params env in
  let return =
    match Env.token env with
    | T_COLON ->
      Env.advance env;
      Some (parse_type env)
    | _ -> None
  in
  let body =
    match Env.token env with
    | T_LEFT_BRACE -> Block (parse_block env)
    | T_EQUALS ->
      Env.advance env;
      Expression (parse_expression env)
    | token -> Parse_error.fatal (Env.loc env, MalformedFunctionBody token)
  in
  let loc = marker env in
  { loc; name; params; body; return; t = () }

and parse_type ?(precedence = TypePrecedence.None) env =
  let open Type in
  let marker = mark_loc env in
  let ty = parse_type_prefix env in
  match Env.token env with
  | T_ARROW when TypePrecedence.(is_tighter Function precedence) ->
    Function (parse_function_type env ty marker)
  | _ -> ty

and parse_type_prefix env =
  let open Type in
  match Env.token env with
  | T_LEFT_PAREN -> parse_group_type env
  | T_UNIT
  | T_INT
  | T_STRING
  | T_BOOL ->
    Primitive (parse_primitive_type env)
  | token -> Parse_error.fatal (Env.loc env, MalformedType token)

and parse_group_type env =
  Env.expect env T_LEFT_PAREN;
  let ty = parse_type env in
  Env.expect env T_RIGHT_PAREN;
  ty

and parse_primitive_type env =
  let open Type.Primitive in
  let kind =
    match Env.token env with
    | T_UNIT -> Unit
    | T_INT -> Int
    | T_STRING -> String
    | T_BOOL -> Bool
    | _ -> failwith "Must be called on primitive type"
  in
  let loc = Env.loc env in
  Env.advance env;
  { loc; kind; t = () }

and parse_function_type env left marker =
  let open Type.Function in
  Env.expect env T_ARROW;
  let rec helper acc =
    let ty = parse_type ~precedence:TypePrecedence.Function env in
    match Env.token env with
    | T_ARROW ->
      Env.advance env;
      helper (ty :: acc)
    | _ -> ty :: acc
  in
  let rev_tys = helper [left] in
  match rev_tys with
  | [] -> failwith "Function type must have at least one type"
  | return :: rev_params ->
    let params = List.rev rev_params in
    let loc = marker env in
    { loc; params; return; t = () }
