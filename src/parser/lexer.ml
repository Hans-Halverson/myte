type t = {
  (* All bytes to tokenize *)
  bytes: Bytes.t;
  (* Source of bytes *)
  source: Source.t option;
  (* Current byte *)
  mutable current: Char.t;
  (* Offset of current byte in bytes *)
  mutable current_offset: int;
  (* Line number of current byte *)
  mutable current_line: int;
  (* Offset of current line in bytes *)
  mutable current_line_offset: int;
}

type result = {
  loc: Loc.t;
  token: Token.t;
}

type tokenize_result =
  | Token of (t * result)
  | LexError of (t * (Loc.t * Parse_error.t))

let eof = Char.unsafe_chr (-1)

let mk source bytes =
  let current =
    if Bytes.length bytes > 0 then
      Bytes.unsafe_get bytes 0
    else
      eof
  in
  { bytes; source; current; current_offset = 0; current_line = 1; current_line_offset = 0 }

let copy lex =
  {
    bytes = lex.bytes;
    source = lex.source;
    current = lex.current;
    current_offset = lex.current_offset;
    current_line = lex.current_line;
    current_line_offset = lex.current_line_offset;
  }

let current_pos lex =
  { Loc.line = lex.current_line; col = lex.current_offset - lex.current_line_offset }

let current_loc lex start_pos =
  { Loc.source = lex.source; start = start_pos; _end = current_pos lex }

let loc_of_pos lex pos =
  { Loc.source = lex.source; start = pos; _end = { line = pos.line; col = pos.col + 1 } }

(* Advance the lexer one byte *)
let advance lex =
  (* If current byte is a new line character, next byte will be on a new line *)
  (match lex.current with
  | '\n' ->
    lex.current_line <- lex.current_line + 1;
    lex.current_line_offset <- lex.current_offset + 1
  | _ -> ());

  (* Only move to next byte if not already at end *)
  let next_offset = lex.current_offset + 1 in
  let bytes_length = Bytes.length lex.bytes in
  if next_offset < bytes_length then (
    lex.current <- Bytes.unsafe_get lex.bytes next_offset;
    lex.current_offset <- next_offset
  ) else (
    lex.current <- eof;
    lex.current_offset <- bytes_length
  )

let advance_two lex =
  advance lex;
  advance lex

let advance_three lex =
  advance lex;
  advance lex;
  advance lex

let advance_four lex =
  advance lex;
  advance lex;
  advance lex;
  advance lex

(* Return the next byte if not at end *)
let peek lex =
  let next_offset = lex.current_offset + 1 in
  if next_offset < Bytes.length lex.bytes then
    Bytes.unsafe_get lex.bytes next_offset
  else
    eof

(* Return byte after the next byte if not at end *)
let peek_two lex =
  let offset = lex.current_offset + 2 in
  if offset < Bytes.length lex.bytes then
    Bytes.unsafe_get lex.bytes offset
  else
    eof

(* Return byte two after the next byte if not at end *)
let peek_three lex =
  let offset = lex.current_offset + 3 in
  if offset < Bytes.length lex.bytes then
    Bytes.unsafe_get lex.bytes offset
  else
    eof

let rec skip_line_comment lex =
  match lex.current with
  | '\n'
  | '\r' ->
    ()
  | current when current = eof -> ()
  | _ ->
    advance lex;
    skip_line_comment lex

let skip_block_comment lex start_pos =
  let rec iter () =
    match lex.current with
    | '*' when peek lex = '/' ->
      advance_two lex;
      Ok ()
    | current when current = eof ->
      Error
        (current_loc lex start_pos, Parse_error.UnexpectedToken { actual = T_EOF; expected = None })
    | _ ->
      advance lex;
      iter ()
  in
  iter ()

(* Only called if current byte is a valid identifier start character *)
let parse_identifier_or_keyword lex =
  let start_pos = current_pos lex in
  let start_offset = lex.current_offset in
  let token_result token = Token (lex, { loc = current_loc lex start_pos; token }) in

  let rec iter () =
    match lex.current with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' ->
      advance lex;
      iter ()
    | _ ->
      let raw = Bytes.sub_string lex.bytes start_offset (lex.current_offset - start_offset) in
      (match raw with
      | "true" -> token_result (T_BOOL_LITERAL true)
      | "false" -> token_result (T_BOOL_LITERAL false)
      | "val" -> token_result T_VAL
      | "var" -> token_result T_VAR
      | "fun" -> token_result T_FUN
      | "fn" -> token_result T_FN
      | "if" -> token_result T_IF
      | "else" -> token_result T_ELSE
      | "while" -> token_result T_WHILE
      | "for" -> token_result T_FOR
      | "in" -> token_result T_IN
      | "return" -> token_result T_RETURN
      | "break" -> token_result T_BREAK
      | "continue" -> token_result T_CONTINUE
      | "match" -> token_result T_MATCH
      | "when" -> token_result T_WHEN
      | "module" -> token_result T_MODULE
      | "import" -> token_result T_IMPORT
      | "as" -> token_result T_AS
      | "type" -> token_result T_TYPE
      | "alias" -> token_result T_ALIAS
      | "builtin" -> token_result T_BUILTIN
      | "trait" -> token_result T_TRAIT
      | "methods" -> token_result T_METHODS
      | "extends" -> token_result T_EXTENDS
      | "implements" -> token_result T_IMPLEMENTS
      | "pub" -> token_result T_PUB
      | "static" -> token_result T_STATIC
      | "override" -> token_result T_OVERRIDE
      | _ -> token_result (T_IDENTIFIER raw))
  in
  (* An underscore not followed by an identifier character is a wildcard token *)
  match lex.current with
  | '_' ->
    (match peek lex with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' ->
      iter ()
    | _ ->
      advance lex;
      token_result T_WILDCARD)
  | _ -> iter ()

let rec iter_dec_digits lex =
  match lex.current with
  | '0' .. '9' ->
    advance lex;
    iter_dec_digits lex
  | _ -> ()

(* Called after decimal sequence when current byte is '.', 'e', or 'E' *)
let parse_number_float lex start_pos start_offset =
  let mk_float_token () =
    let raw = Bytes.sub_string lex.bytes start_offset (lex.current_offset - start_offset) in
    Token (lex, { loc = current_loc lex start_pos; token = T_FLOAT_LITERAL raw })
  in

  (* Period is optional, and optionally followed by any number of digits *)
  (match lex.current with
  | '.' ->
    advance lex;
    iter_dec_digits lex
  | _ -> ());

  (* Exponent is optional *)
  match lex.current with
  | 'e'
  | 'E' ->
    advance lex;

    (* Exponent optionally starts with sign *)
    (match lex.current with
    | '+'
    | '-' ->
      advance lex
    | _ -> ());

    (* Exponent must contain at least one digit *)
    (match lex.current with
    | '0' .. '9' ->
      advance lex;

      (* Exponent may contain any number of remaining digits *)
      iter_dec_digits lex;

      mk_float_token ()
    | _ -> LexError (lex, (current_loc lex start_pos, Parse_error.MalformedFloatLiteral)))
  | _ -> mk_float_token ()

let parse_number_dec lex =
  let start_pos = current_pos lex in
  let start_offset = lex.current_offset in

  let mk_int_token () =
    let raw = Bytes.sub_string lex.bytes start_offset (lex.current_offset - start_offset) in
    Token (lex, { loc = current_loc lex start_pos; token = T_INT_LITERAL (raw, Integers.Dec) })
  in

  (* Find all digits in number *)
  iter_dec_digits lex;

  match lex.current with
  | '.' ->
    (* Period indicates this is a float literal or named access  *)
    (match peek lex with
    (* Start of identifier means this is be a named access. This includes when name starts with an
       'e', since floats must have at least one digit between the period and exponent *)
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '_' ->
      mk_int_token ()
    | _ -> parse_number_float lex start_pos start_offset)
  (* Exponent indicates this is a float literal *)
  | 'e'
  | 'E' ->
    parse_number_float lex start_pos start_offset
  | _ -> mk_int_token ()

let parse_number_hex lex =
  let start_pos = current_pos lex in
  let start_offset = lex.current_offset in

  (* Skip `0x` prefix *)
  advance_two lex;

  (* Find all hex digits in number *)
  let rec iter () =
    match lex.current with
    | '0' .. '9'
    | 'a' .. 'f'
    | 'A' .. 'F' ->
      advance lex;
      iter ()
    | _ -> ()
  in
  iter ();

  let raw = Bytes.sub_string lex.bytes start_offset (lex.current_offset - start_offset) in
  Token (lex, { loc = current_loc lex start_pos; token = T_INT_LITERAL (raw, Integers.Hex) })

let parse_number_bin lex =
  let start_pos = current_pos lex in
  let start_offset = lex.current_offset in

  (* Skip `0b` prefix *)
  advance_two lex;

  (* Find all binary digits in number *)
  let rec iter () =
    match lex.current with
    | '0'
    | '1' ->
      advance lex;
      iter ()
    | _ -> ()
  in
  iter ();

  let raw = Bytes.sub_string lex.bytes start_offset (lex.current_offset - start_offset) in
  Token (lex, { loc = current_loc lex start_pos; token = T_INT_LITERAL (raw, Integers.Bin) })

let parse_string_literal lex ~is_interpolated ~start_pos =
  let builder = Buffer.create 10 in
  let token_result token = Token (lex, { loc = current_loc lex start_pos; token }) in
  let rec iter () =
    match lex.current with
    (* Double quotes are end for string literal *)
    | '"' when not is_interpolated ->
      advance lex;
      token_result (Token.T_STRING_LITERAL (Buffer.contents builder))
    (* Backtick is end for interpolated string literal *)
    | '`' when is_interpolated ->
      advance lex;
      token_result (Token.T_INTERPOLATED_STRING (Buffer.contents builder, true))
    (* `${` marks the beginning of an expression in an interpolated string *)
    | '$' when is_interpolated ->
      (match peek lex with
      | '{' ->
        advance_two lex;
        token_result (Token.T_INTERPOLATED_STRING (Buffer.contents builder, false))
      | _ -> add_char_and_advance '$')
    (* Newlines are allowed in interpolated strings, but not normal string literals *)
    | '\n' when not is_interpolated ->
      LexError (lex, (loc_of_pos lex start_pos, Parse_error.UnterminatedStringLiteral))
    | '\\' ->
      let start_pos = current_pos lex in
      advance lex;
      parse_escape_sequence start_pos
    | current when current = eof ->
      LexError (lex, (loc_of_pos lex start_pos, Parse_error.UnterminatedStringLiteral))
    | current -> add_char_and_advance current
  and parse_escape_sequence start_pos =
    let invalid_escape_error () =
      LexError (lex, (loc_of_pos lex (current_pos lex), Parse_error.InvalidEscape is_interpolated))
    in
    match lex.current with
    (* Escape sequences for both string literals and interpolated strings *)
    | 'n' -> add_char_and_advance '\n'
    | 'r' -> add_char_and_advance '\r'
    | 't' -> add_char_and_advance '\t'
    | '\\' -> add_char_and_advance '\\'
    | 'x' ->
      advance lex;
      parse_hex_escape_sequence start_pos
    (* Double quotes are only an escape sequence in string literals *)
    | '"' ->
      if is_interpolated then
        invalid_escape_error ()
      else
        add_char_and_advance '"'
    (* Backtick is only an escape sequence in interpolated strings *)
    | '`' ->
      if not is_interpolated then
        invalid_escape_error ()
      else
        add_char_and_advance '`'
    (* Dollar sign is only an escape sequence in interpolated strings *)
    | '$' ->
      if not is_interpolated then
        invalid_escape_error ()
      else
        add_char_and_advance '$'
    | _ -> invalid_escape_error ()
  and parse_hex_escape_sequence start_pos =
    let int_of_hex_digit digit =
      if digit >= 'a' && digit <= 'f' then
        Char.code digit - Char.code 'a' + 10
      else if digit >= 'A' && digit <= 'F' then
        Char.code digit - Char.code 'A' + 10
      else
        Char.code digit - Char.code '0'
    in
    let err () = LexError (lex, (current_loc lex start_pos, Parse_error.InvalidHexEscape)) in
    match lex.current with
    | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as first_digit ->
      advance lex;
      (match lex.current with
      | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as second_digit ->
        let char_code = (int_of_hex_digit first_digit * 16) + int_of_hex_digit second_digit in
        add_char_and_advance (Char.chr char_code)
      | _ -> err ())
    | _ -> err ()
  and add_char_and_advance char =
    advance lex;
    Buffer.add_char builder char;
    iter ()
  in
  iter ()

let parse_char_literal lex =
  let start_pos = current_pos lex in
  advance lex;
  let malformed_err () =
    LexError (lex, (loc_of_pos lex (current_pos lex), Parse_error.MalformedCharLiteral))
  in
  let advance_char_literal value =
    advance lex;
    match lex.current with
    | '\'' ->
      advance lex;
      Token (lex, { loc = current_loc lex start_pos; token = T_CHAR_LITERAL value })
    | _ -> malformed_err ()
  in
  match lex.current with
  (* Unescaped single quote is not allowed *)
  | '\'' -> malformed_err ()
  (* Escape sequences for char literals *)
  | '\\' ->
    advance lex;
    (match lex.current with
    | '\'' -> advance_char_literal '\''
    | '\\' -> advance_char_literal '\\'
    | 'n' -> advance_char_literal '\n'
    | 'r' -> advance_char_literal '\r'
    | 't' -> advance_char_literal '\t'
    | _ -> LexError (lex, (loc_of_pos lex (current_pos lex), Parse_error.InvalidCharEscape)))
  (* All other printable chars are allowed *)
  | ' ' .. '~' as value -> advance_char_literal value
  | _ -> malformed_err ()

let rec tokenize lex =
  let open Token in
  let rec skip_whitespace () =
    match lex.current with
    | '\t'
    | '\r'
    | '\n'
    | ' ' ->
      advance lex;
      skip_whitespace ()
    | _ -> ()
  in
  skip_whitespace ();

  let start_pos = current_pos lex in
  let token_result token = Token (lex, { loc = current_loc lex start_pos; token }) in
  let lex_error err = LexError (lex, (current_loc lex start_pos, err)) in

  match lex.current with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' ->
    parse_identifier_or_keyword lex
  | '0'
    when peek lex = 'x'
         &&
         let char = peek_two lex in
         match char with
         | '0' .. '9'
         | 'a' .. 'f'
         | 'A' .. 'F' ->
           true
         | _ -> false ->
    parse_number_hex lex
  | '0'
    when peek lex = 'b'
         &&
         let char = peek_two lex in
         char = '0' || char = '1' ->
    parse_number_bin lex
  | '0' .. '9' -> parse_number_dec lex
  | ';' ->
    advance lex;
    token_result T_SEMICOLON
  | ':' ->
    advance lex;
    token_result T_COLON
  | '?' ->
    advance lex;
    token_result T_QUESTION
  | '.' ->
    advance lex;
    token_result T_PERIOD
  | ',' ->
    advance lex;
    token_result T_COMMA
  | '(' ->
    advance lex;
    token_result T_LEFT_PAREN
  | ')' ->
    advance lex;
    token_result T_RIGHT_PAREN
  | '}' ->
    advance lex;
    token_result T_RIGHT_BRACE
  | '[' ->
    advance lex;
    token_result T_LEFT_BRACKET
  | ']' ->
    advance lex;
    token_result T_RIGHT_BRACKET
  | '@' ->
    advance lex;
    token_result T_AT
  | '/' ->
    (match peek lex with
    | '/' ->
      advance_two lex;
      skip_line_comment lex;
      tokenize lex
    | '*' ->
      let start_pos = current_pos lex in
      advance_two lex;
      (match skip_block_comment lex start_pos with
      | Ok _ -> tokenize lex
      | Error err -> LexError (lex, err))
    | '=' ->
      advance_two lex;
      token_result T_DIVIDE_EQUALS
    | _ ->
      advance lex;
      token_result T_DIVIDE)
  | '+' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_PLUS_EQUALS
    | _ ->
      advance lex;
      token_result T_PLUS)
  | '-' ->
    (match peek lex with
    | '>' ->
      advance_two lex;
      token_result T_ARROW
    | '=' ->
      advance_two lex;
      token_result T_MINUS_EQUALS
    | _ ->
      advance lex;
      token_result T_MINUS)
  | '*' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_MULTIPLY_EQUALS
    | _ ->
      advance lex;
      token_result T_MULTIPLY)
  | '%' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_PERCENT_EQUALS
    | _ ->
      advance lex;
      token_result T_PERCENT)
  | '^' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_CARET_EQUALS
    | _ ->
      advance lex;
      token_result T_CARET)
  | '{' ->
    (match peek lex with
    | '|' ->
      advance_two lex;
      token_result T_SET_OPEN
    | _ ->
      advance lex;
      token_result T_LEFT_BRACE)
  | '&' ->
    (match peek lex with
    | '&' ->
      advance_two lex;
      token_result T_LOGICAL_AND
    | '=' ->
      advance_two lex;
      token_result T_AMPERSAND_EQUALS
    | _ ->
      advance lex;
      token_result T_AMPERSAND)
  | '|' ->
    (match peek lex with
    | '|' ->
      advance_two lex;
      token_result T_LOGICAL_OR
    | '}' ->
      advance_two lex;
      token_result T_SET_CLOSE
    | '=' ->
      advance_two lex;
      token_result T_PIPE_EQUALS
    | _ ->
      advance lex;
      token_result T_PIPE)
  | '=' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_DOUBLE_EQUALS
    | _ ->
      advance lex;
      token_result T_EQUALS)
  | '!' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_NOT_EQUALS
    | _ ->
      advance lex;
      token_result T_BANG)
  | '<' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_LESS_THAN_OR_EQUAL
    | '<' ->
      (match peek_two lex with
      | '=' ->
        advance_three lex;
        token_result T_LEFT_SHIFT_EQUALS
      | _ ->
        advance lex;
        token_result T_LESS_THAN)
    | _ ->
      advance lex;
      token_result T_LESS_THAN)
  | '>' ->
    (match peek lex with
    | '=' ->
      advance_two lex;
      token_result T_GREATER_THAN_OR_EQUAL
    | '>' ->
      (match peek_two lex with
      | '=' ->
        advance_three lex;
        token_result T_ARITHMETIC_RIGHT_SHIFT_EQUALS
      | '>' ->
        (match peek_three lex with
        | '=' ->
          advance_four lex;
          token_result T_LOGICAL_RIGHT_SHIFT_EQUALS
        | _ ->
          advance lex;
          token_result T_GREATER_THAN)
      | _ ->
        advance lex;
        token_result T_GREATER_THAN)
    | _ ->
      advance lex;
      token_result T_GREATER_THAN)
  | '"' ->
    let start_pos = current_pos lex in
    advance lex;
    parse_string_literal lex ~is_interpolated:false ~start_pos
  | '`' ->
    let start_pos = current_pos lex in
    advance lex;
    parse_string_literal lex ~is_interpolated:true ~start_pos
  | '\'' -> parse_char_literal lex
  | current when current = eof -> token_result T_EOF
  | current ->
    advance lex;
    lex_error (Parse_error.UnknownToken (String.make 1 current))

let next lexer =
  match tokenize lexer with
  | Token (lexer, result) -> (lexer, Ok result)
  | LexError (lexer, result) -> (lexer, Error result)

let next_in_interpolated_string lexer =
  (* Adjust pos to start at '}' which must precede this call *)
  let current_pos = current_pos lexer in
  let start_pos = { Loc.line = current_pos.line; col = current_pos.col - 1 } in
  match parse_string_literal ~is_interpolated:true ~start_pos lexer with
  | Token (lexer, result) -> (lexer, Ok result)
  | LexError (lexer, result) -> (lexer, Error result)
