module Env = struct
  type t = {
    mutable lexer: Lexer.t;
    mutable lex_result: (Lexer.result, Loc.t * Parse_error.t) result;
    mutable prev_lex_result: (Lexer.result, Loc.t * Parse_error.t) result option;
    mutable errors: (Loc.t * Parse_error.t) list;
    (* A stack of bools where each bool represents whether bitwise or is not allowed
       in the enclosing matches *)
    mutable match_stack: bool Stack.t;
    (* Whether the lexer should currently be lexing an interpolated string *)
    mutable in_interpolated_string: bool;
  }

  let rec mk lexer =
    let (lexer, lex_result) = Lexer.next lexer in
    Result.iter_error Parse_error.fatal lex_result;
    let match_stack = Stack.create () in
    Stack.push false match_stack;
    {
      lexer;
      lex_result;
      prev_lex_result = None;
      errors = [];
      match_stack;
      in_interpolated_string = false;
    }

  and lex_result env =
    match env.lex_result with
    | Ok result -> result
    | Error err -> Parse_error.fatal err

  and loc env = (lex_result env).loc

  and token env = (lex_result env).token

  and errors env = List.rev env.errors

  and prev_loc env =
    match env.prev_lex_result with
    | Some (Ok { Lexer.loc; _ }) -> loc
    | _ -> failwith "No previous location"

  and lexer_next env =
    if env.in_interpolated_string then
      Lexer.next_in_interpolated_string env.lexer
    else
      Lexer.next env.lexer

  and advance env =
    let (lexer, new_lex_result) = lexer_next env in
    let prev_lex_result = env.lex_result in
    env.prev_lex_result <- Some prev_lex_result;
    env.lex_result <- new_lex_result;
    Result.iter_error Parse_error.fatal new_lex_result;
    env.lexer <- lexer;
    (* Track whether whether parser is in top level of match statement *)
    if not env.in_interpolated_string then
      match (Result.get_ok prev_lex_result).token with
      | T_LEFT_PAREN
      | T_LEFT_BRACE
      | T_LEFT_BRACKET ->
        Stack.push false env.match_stack
      | T_RIGHT_PAREN
      | T_RIGHT_BRACE
      | T_RIGHT_BRACKET ->
        ignore (Stack.pop env.match_stack)
      | _ -> ()

  and expect env expected =
    let actual = token env in
    if actual <> expected then
      Parse_error.fatal (loc env, UnexpectedToken { actual; expected = Some expected });
    advance env

  and error env error = env.errors <- error :: env.errors

  and peek env =
    let saved_lexer = Lexer.deep_copy env.lexer in
    let (loc, token) =
      match lexer_next env with
      | (_, Ok { loc; token }) -> (loc, token)
      | (_, Error err) -> Parse_error.fatal err
    in
    env.lexer <- saved_lexer;
    (loc, token)

  and enter_match env = Stack.push true env.match_stack

  and exit_match env = ignore (Stack.pop env.match_stack)

  and can_use_bitwise_or env = not (Stack.top env.match_stack)

  and enter_interpolated_string env = env.in_interpolated_string <- true

  and exit_interpolated_string env = env.in_interpolated_string <- false
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
