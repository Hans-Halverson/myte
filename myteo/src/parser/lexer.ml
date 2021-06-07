let new_line = [%sedlex.regexp? '\n' | '\r' | "\r\n"]

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let digit = [%sedlex.regexp? '0' .. '9']

let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']

let bin_digit = [%sedlex.regexp? '0' .. '1']

let identifier = [%sedlex.regexp? ((letter | '_'), Star (letter | digit | '_'))]

let dec_literal = [%sedlex.regexp? Plus digit]

let hex_literal = [%sedlex.regexp? ("0x", Plus hex_digit)]

let bin_literal = [%sedlex.regexp? ("0b", Plus bin_digit)]

let string_literal = [%sedlex.regexp? ('"', Star (Compl '"'), '"')]

let lexeme = Sedlexing.Utf8.lexeme

type t = {
  buf: Sedlexing.lexbuf;
  source: Source.t option;
  current_line: int;
  current_line_offset: int;
}

type result = {
  loc: Loc.t;
  token: Token.t;
}

type tokenize_result =
  | Token of (t * result)
  | Skip of t
  | LexError of (t * (Loc.t * Parse_error.t))

let mk source buf = { buf; source; current_line = 1; current_line_offset = 0 }

let pos_of_offset lex offset =
  { Loc.line = lex.current_line; col = offset - lex.current_line_offset }

let current_loc lex =
  let (start_offset, end_offset) = Sedlexing.loc lex.buf in
  {
    Loc.source = lex.source;
    start = pos_of_offset lex start_offset;
    _end = pos_of_offset lex end_offset;
  }

let mark_new_line lex =
  let new_current_line_offset = Sedlexing.lexeme_end lex.buf in
  { lex with current_line = lex.current_line + 1; current_line_offset = new_current_line_offset }

let rec skip_line_comment lex =
  let { buf; _ } = lex in
  match%sedlex buf with
  | eof
  | new_line ->
    Skip (mark_new_line lex)
  | any -> skip_line_comment lex
  | _ -> failwith "Unreachable"

let rec skip_block_comment lex =
  let { buf; _ } = lex in
  match%sedlex buf with
  | "*/" -> Skip lex
  | eof ->
    LexError
      (lex, (current_loc lex, Parse_error.UnexpectedToken { actual = T_EOF; expected = None }))
  | new_line -> skip_block_comment (mark_new_line lex)
  | any -> skip_block_comment lex
  | _ -> failwith "Unreachable"

let tokenize lex =
  let open Token in
  let { buf; _ } = lex in
  let token_result token = Token (lex, { loc = current_loc lex; token }) in
  let lex_error lex = LexError (lex, (current_loc lex, Parse_error.UnknownToken (lexeme buf))) in
  match%sedlex buf with
  | new_line -> Skip (mark_new_line lex)
  | white_space -> Skip lex
  | "//" -> skip_line_comment lex
  | "/*" -> skip_block_comment lex
  | "&&" -> token_result T_LOGICAL_AND
  | "||" -> token_result T_LOGICAL_OR
  | "==" -> token_result T_DOUBLE_EQUALS
  | "!=" -> token_result T_NOT_EQUALS
  | "<" -> token_result T_LESS_THAN
  | ">" -> token_result T_GREATER_THAN
  | "<=" -> token_result T_LESS_THAN_OR_EQUAL
  | ">=" -> token_result T_GREATER_THAN_OR_EQUAL
  | "->" -> token_result T_ARROW
  | ';' -> token_result T_SEMICOLON
  | ':' -> token_result T_COLON
  | '?' -> token_result T_QUESTION
  | '|' -> token_result T_PIPE
  | '.' -> token_result T_PERIOD
  | ',' -> token_result T_COMMA
  | '=' -> token_result T_EQUALS
  | '+' -> token_result T_PLUS
  | '-' -> token_result T_MINUS
  | '*' -> token_result T_MULTIPLY
  | '/' -> token_result T_DIVIDE
  | '!' -> token_result T_LOGICAL_NOT
  | '(' -> token_result T_LEFT_PAREN
  | ')' -> token_result T_RIGHT_PAREN
  | '{' -> token_result T_LEFT_BRACE
  | '}' -> token_result T_RIGHT_BRACE
  | '[' -> token_result T_LEFT_BRACKET
  | ']' -> token_result T_RIGHT_BRACKET
  | "true" -> token_result (T_BOOL_LITERAL true)
  | "false" -> token_result (T_BOOL_LITERAL false)
  | "val" -> token_result T_VAL
  | "var" -> token_result T_VAR
  | "fun" -> token_result T_FUN
  | "if" -> token_result T_IF
  | "else" -> token_result T_ELSE
  | "while" -> token_result T_WHILE
  | "return" -> token_result T_RETURN
  | "break" -> token_result T_BREAK
  | "continue" -> token_result T_CONTINUE
  | "module" -> token_result T_MODULE
  | "import" -> token_result T_IMPORT
  | "as" -> token_result T_AS
  | "type" -> token_result T_TYPE
  | "alias" -> token_result T_ALIAS
  | "unit" -> token_result T_UNIT
  | "byte" -> token_result T_BYTE
  | "int" -> token_result T_INT
  | "long" -> token_result T_LONG
  | "string" -> token_result T_STRING
  | "bool" -> token_result T_BOOL
  | eof -> token_result T_EOF
  | identifier -> token_result (T_IDENTIFIER (lexeme buf))
  | dec_literal ->
    let raw = lexeme buf in
    token_result (T_INT_LITERAL (raw, Integers.Dec))
  | bin_literal ->
    let raw = lexeme buf in
    token_result (T_INT_LITERAL (raw, Integers.Bin))
  | hex_literal ->
    let raw = lexeme buf in
    token_result (T_INT_LITERAL (raw, Integers.Hex))
  | string_literal ->
    let raw = lexeme buf in
    let value = String.sub raw 1 (String.length raw - 2) in
    token_result (T_STRING_LITERAL value)
  | _ -> lex_error lex

let next lexer =
  let rec find_next_token lexer =
    match tokenize lexer with
    | Skip lexer -> find_next_token lexer
    | Token (lexer, result) -> (lexer, Ok result)
    | LexError (lexer, result) -> (lexer, Error result)
  in
  find_next_token lexer
