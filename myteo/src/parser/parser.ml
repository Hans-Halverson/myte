open Ast
open Token

module Env = struct
  type t = {
    mutable lexer: Lexer.t;
    mutable lex_result: Lexer.result;
  }

  let mk lexer =
    let (lexer, lex_result) = Lexer.next lexer in
    { lexer; lex_result }

  let loc env = env.lex_result.loc

  let peek env = env.lex_result.token

  let advance env =
    let (lexer, lex_result) = Lexer.next env.lexer in
    env.lex_result <- lex_result;
    env.lexer <- lexer

  let expect env expected =
    let actual = peek env in
    if actual <> expected then
      Parse_error.fatal (Parse_error.(UnexpectedToken {actual; expected = Some expected}));
    advance env
end

let rec parse env =
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
  let expr = parse_expression env in
  Env.expect env T_SEMICOLON;
  Statement.Expression expr

and parse_expression env =
  let open Expression in
  match Env.peek env with
  | T_IDENTIFIER _ -> Identifier (parse_identifier env)
  | token -> Parse_error.(fatal (UnexpectedToken { actual = token; expected = None }))

and parse_identifier env =
  match Env.peek env with
  | T_IDENTIFIER name ->
    let loc = Env.loc env in
    Env.advance env;
    { Identifier.loc; name; t = () }
  | _ -> assert false


let parse_file file =
  let file_chan = open_in file in
  let buf = Sedlexing.Utf8.from_channel file_chan in
  let lexer = Lexer.mk (Some file) buf in
  let env = Env.mk lexer in
  parse env

let parse_string str =
  let buf = Sedlexing.Utf8.from_string str in
  let lexer = Lexer.mk None buf in
  let env = Env.mk lexer in
  parse env