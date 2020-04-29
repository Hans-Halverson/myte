module Env = struct
  type t = {
    mutable lexer: Lexer.t;
    mutable lex_result: Lexer.result;
    mutable prev_lex_result: Lexer.result option;
  }

  let mk lexer =
    let (lexer, lex_result) = Lexer.next lexer in
    { lexer; lex_result; prev_lex_result = None }

  let loc env = env.lex_result.loc

  let peek env = env.lex_result.token

  let prev_loc env = (Option.get env.prev_lex_result).loc

  let advance env =
    let (lexer, lex_result) = Lexer.next env.lexer in
    env.prev_lex_result <- Some env.lex_result;
    env.lex_result <- lex_result;
    env.lexer <- lexer

  let expect env expected =
    let actual = peek env in
    if actual <> expected then
      Parse_error.fatal (Parse_error.(UnexpectedToken {actual; expected = Some expected}));
    advance env
end

let from_file file =
  let file_chan = open_in file in
  let buf = Sedlexing.Utf8.from_channel file_chan in
  let lexer = Lexer.mk (Some file) buf in
  Env.mk lexer

let from_string str =
  let buf = Sedlexing.Utf8.from_string str in
  let lexer = Lexer.mk None buf in
  Env.mk lexer