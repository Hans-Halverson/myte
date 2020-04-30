module Env = struct
  type t = {
    mutable lexer: Lexer.t;
    mutable lex_result: (Lexer.result, (Loc.t * Parse_error.t)) result;
    mutable prev_lex_result: (Lexer.result, (Loc.t * Parse_error.t)) result  option;
    mutable errors: (Loc.t * Parse_error.t) list;
  }

  let rec mk lexer =
    let (lexer, lex_result) = Lexer.next lexer in
    { lexer; lex_result; prev_lex_result = None; errors = [] }

  and lex_result env = match env.lex_result with
    | Ok result -> result
    | Error err -> Parse_error.fatal err

  and loc env = (lex_result env).loc

  and token env = (lex_result env).token

  and errors env = List.rev env.errors

  and prev_loc env =
    match env.prev_lex_result with
    | Some (Ok { Lexer.loc; _ }) -> loc
    | _ -> failwith "No previous location"

  and advance env =
    let (lexer, lex_result) = Lexer.next env.lexer in
    Result.iter_error Parse_error.fatal lex_result;
    env.prev_lex_result <- Some env.lex_result;
    env.lex_result <- lex_result;
    env.lexer <- lexer

  and expect env expected =
    let actual = token env in
    if actual <> expected then
      Parse_error.fatal (loc env, UnexpectedToken {actual; expected = Some expected});
    advance env

  and error env error =
    env.errors <- error :: env.errors
end

let from_file file =
  let file_chan = open_in file in
  let buf = Sedlexing.Utf8.from_channel file_chan in
  let lexer = Lexer.mk (Some (File file)) buf in
  Env.mk lexer

let from_string str =
  let buf = Sedlexing.Utf8.from_string str in
  let lexer = Lexer.mk (Some (String str)) buf in
  Env.mk lexer