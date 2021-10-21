module Env = struct
  type t = {
    mutable lexer: Lexer.t;
    mutable lex_result: Lexer.result;
    (* Previous lex result, initially the same as the first lex result until Lexer.next is called
       a second time. *)
    mutable prev_lex_result: Lexer.result;
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
    let lex_result = Result.get_ok lex_result in
    let match_stack = Stack.create () in
    Stack.push false match_stack;
    {
      lexer;
      lex_result;
      prev_lex_result = lex_result;
      errors = [];
      match_stack;
      in_interpolated_string = false;
    }

  and loc env = env.lex_result.loc

  and token env = env.lex_result.token

  and errors env = List.rev env.errors

  and prev_loc env = env.prev_lex_result.loc

  and lexer_next env =
    if env.in_interpolated_string then
      Lexer.next_in_interpolated_string env.lexer
    else
      Lexer.next env.lexer

  and advance env =
    let (lexer, new_lex_result) = lexer_next env in
    Result.iter_error Parse_error.fatal new_lex_result;
    let prev_lex_result = env.lex_result in
    env.prev_lex_result <- prev_lex_result;
    env.lex_result <- Result.get_ok new_lex_result;
    env.lexer <- lexer;
    (* Track whether whether parser is in top level of match statement *)
    if not env.in_interpolated_string then
      match prev_lex_result.token with
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
    let saved_lexer = Lexer.copy env.lexer in
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

let bytes_of_file file =
  let file_chan = open_in_bin file in
  let file_chan_length = in_channel_length file_chan in
  let file_bytes = Bytes.create file_chan_length in
  really_input file_chan file_bytes 0 file_chan_length;
  close_in file_chan;
  file_bytes

let from_file file =
  let bytes = bytes_of_file file in
  let lexer = Lexer.mk (Some (File file)) bytes in
  Env.mk lexer

let from_string str =
  let lexer = Lexer.mk (Some (String str)) (Bytes.of_string str) in
  Env.mk lexer
