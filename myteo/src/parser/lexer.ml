let new_line = [%sedlex.regexp? '\n' | '\r' | "\r\n" ]

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']

let digit = [%sedlex.regexp? '0'..'9']

let identifier = [%sedlex.regexp? (letter | '_'), Star (letter | digit | '_')]

let int_literal = [%sedlex.regexp? Plus digit]

let string_literal = [%sedlex.regexp? '"', Star (Compl '"'), '"']

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

type tokenize_result =
  | Token of (t * result)
  | Whitespace of t

let mk file buf = { buf; file; current_line = 1; current_line_offset = 0 }

let pos_of_offset lex offset =
  { Loc.line = lex.current_line; col = offset - lex.current_line_offset }

let current_loc lex =
  let (start_offset, end_offset) = Sedlexing.loc lex.buf in
  { Loc.file = lex.file; start = pos_of_offset lex start_offset; _end = pos_of_offset lex end_offset }

let mark_new_line lex =
  let new_current_line_offset = Sedlexing.lexeme_end lex.buf in
  {
    lex with
    current_line = lex.current_line + 1;
    current_line_offset = new_current_line_offset
  }

let tokenize lex =
  let open Token in
  let { buf; _ } = lex in
  let token_result token = Token (lex, { loc = current_loc lex; token }) in
  match %sedlex buf with
  | new_line -> Whitespace (mark_new_line lex)
  | white_space -> Whitespace lex
  | ';' -> token_result T_SEMICOLON
  | '+' -> token_result T_PLUS
  | '-' -> token_result T_MINUS
  | '*' -> token_result T_MULTIPLY
  | '/' -> token_result T_DIVIDE
  | '!' -> token_result T_LOGICAL_NOT
  | "&&" -> token_result T_LOGICAL_AND
  | "||" -> token_result T_LOGICAL_OR
  | "true" -> token_result (T_BOOL_LITERAL true)
  | "false" -> token_result (T_BOOL_LITERAL false)
  | eof -> token_result T_EOF
  | identifier -> token_result (T_IDENTIFIER (lexeme buf))
  | int_literal ->
    let raw = lexeme buf in
    token_result (T_INT_LITERAL (int_of_string raw, raw))
  | string_literal ->
    let raw = lexeme buf in
    let value = String.sub raw 1 (String.length raw - 2) in
    token_result (T_STRING_LITERAL value)
  | _ -> Parse_error.(fatal (UnknownToken (lexeme buf)))

let next lexer =
  let rec skip_whitespace lexer =
    match tokenize lexer with
    | Whitespace lexer -> skip_whitespace lexer
    | Token result -> result
  in
  skip_whitespace lexer
