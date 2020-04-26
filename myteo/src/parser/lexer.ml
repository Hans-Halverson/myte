let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']

let digit = [%sedlex.regexp? '0'..'9']

let identifier = [%sedlex.regexp? Plus (letter | digit | '_')]

let lexeme = Sedlexing.Utf8.lexeme

type t = {
  buf: Sedlexing.lexbuf;
  file: string option;
  current_line: int;
  current_line_offset: int;
}

type result = {
  loc: Loc.t;
  token: Token.t;
}

let mk file buf = { buf; file; current_line = 1; current_line_offset = 0 }

let pos_of_offset lex offset =
  { Loc.line = lex.current_line; col = offset - lex.current_line_offset }

let current_loc lex =
  let (start_offset, end_offset) = Sedlexing.loc lex.buf in
  { Loc.file = lex.file; start = pos_of_offset lex start_offset; _end = pos_of_offset lex end_offset }

let rec tokenize lex =
  let open Token in
  let { buf; _ } = lex in
  let token_result token = (lex, current_loc lex, token) in
  match%sedlex buf with
  | '\n' ->
    let new_current_line_offset = Sedlexing.lexeme_end buf in
    let lex = {
      lex with
      current_line = lex.current_line + 1;
      current_line_offset = new_current_line_offset
    } in
    tokenize lex
  | Plus white_space -> tokenize lex
  | identifier -> token_result (T_IDENTIFIER (lexeme buf))
  | ';' -> token_result T_SEMICOLON
  | eof -> token_result T_EOF
  | _ -> Parse_error.(fatal (UnknownToken (lexeme buf)))

let next lex =
  let (lexer, loc, token) = tokenize lex in
  (lexer, { loc; token })
