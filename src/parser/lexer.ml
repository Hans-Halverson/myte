let new_line = [%sedlex.regexp? '\n' | '\r' | "\r\n"]

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let digit = [%sedlex.regexp? '0' .. '9']

let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']

let bin_digit = [%sedlex.regexp? '0' .. '1']

(* Any sequence of letters, digits, and underscores. Does not include the wildcard pattern "_"
   or any sequence starting with a digit *)
let identifier =
  [%sedlex.regexp? ('_', Plus (letter | digit | '_')) | (letter, Star (letter | digit | '_'))]

let dec_literal = [%sedlex.regexp? Plus digit]

let hex_literal = [%sedlex.regexp? ("0x", Plus hex_digit)]

let bin_literal = [%sedlex.regexp? ("0b", Plus bin_digit)]

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

(* Sedlexing has mutable state but does not expose its internals, so use force a sketchy deep copy
   with Obj so we can save and restore the exact state of a Sedlexing buffer. *)
let deep_copy lex = { lex with buf = Obj.repr lex.buf |> Obj.dup |> Obj.obj }

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

let sedlex_catch_all_case () = failwith "Needed for sedlex, but any case catches all patterns"

let parse_string_literal lex ~is_interpolated =
  let start_loc = current_loc lex in
  let builder = Buffer.create 10 in

  let rec iter lex =
    let { buf; _ } = lex in
    match%sedlex buf with
    (* Double quotes are end for string literal *)
    | '"' ->
      if is_interpolated then (
        Buffer.add_string builder (lexeme buf);
        iter lex
      ) else
        let loc = Loc.between start_loc (current_loc lex) in
        let token = Token.T_STRING_LITERAL (Buffer.contents builder) in
        Token (lex, { loc; token })
    (* Backtick is end for interpolated string literal *)
    | '`' ->
      if not is_interpolated then (
        Buffer.add_string builder (lexeme buf);
        iter lex
      ) else
        let loc = Loc.between start_loc (current_loc lex) in
        let token = Token.T_INTERPOLATED_STRING (Buffer.contents builder, true) in
        Token (lex, { loc; token })
    (* `${` marks the beginning of an expression in an interpolated string *)
    | "${" ->
      if not is_interpolated then (
        Buffer.add_string builder (lexeme buf);
        iter lex
      ) else
        let loc = Loc.between start_loc (current_loc lex) in
        let token = Token.T_INTERPOLATED_STRING (Buffer.contents builder, false) in
        Token (lex, { loc; token })
    (* Newlines are allowed in interpolated strings, but not normal string literals *)
    | '\n' ->
      if is_interpolated then (
        Buffer.add_string builder (lexeme buf);
        iter lex
      ) else
        LexError (lex, (start_loc, Parse_error.UnterminatedStringLiteral))
    | eof -> LexError (lex, (start_loc, Parse_error.UnterminatedStringLiteral))
    | '\\' -> parse_escape_sequence lex
    | any ->
      Buffer.add_string builder (lexeme buf);
      iter lex
    | _ -> sedlex_catch_all_case ()
  and parse_escape_sequence lex =
    let { buf; _ } = lex in
    let invalid_escape_error lex is_interpolated =
      LexError (lex, (current_loc lex, Parse_error.InvalidEscape is_interpolated))
    in
    let start_loc = current_loc lex in
    match%sedlex buf with
    (* Escape sequences for both string literals and interpolated strings *)
    | 'n' ->
      Buffer.add_char builder '\n';
      iter lex
    | 'r' ->
      Buffer.add_char builder '\r';
      iter lex
    | 't' ->
      Buffer.add_char builder '\t';
      iter lex
    | '\\' ->
      Buffer.add_char builder '\\';
      iter lex
    | 'x' -> parse_hex_escape_sequence lex start_loc
    (* Double quotes are only an escape sequence in string literals *)
    | '"' ->
      if is_interpolated then
        invalid_escape_error lex is_interpolated
      else (
        Buffer.add_char builder '"';
        iter lex
      )
    (* Backtick is only an escape sequence in interpolated strings *)
    | '`' ->
      if not is_interpolated then
        invalid_escape_error lex is_interpolated
      else (
        Buffer.add_char builder '`';
        iter lex
      )
    (* Dollar sign is only an escape sequence in interpolated strings *)
    | '$' ->
      if not is_interpolated then
        invalid_escape_error lex is_interpolated
      else (
        Buffer.add_char builder '$';
        iter lex
      )
    | _ -> invalid_escape_error lex is_interpolated
  and parse_hex_escape_sequence lex start_loc =
    let { buf; _ } = lex in
    let int_of_hex_digit digit =
      if digit >= 'a' && digit <= 'f' then
        Char.code digit - Char.code 'a' + 10
      else if digit >= 'A' && digit <= 'F' then
        Char.code digit - Char.code 'A' + 10
      else
        Char.code digit - Char.code '0'
    in
    let err () =
      LexError (lex, (Loc.between start_loc (current_loc lex), Parse_error.InvalidHexEscape))
    in
    match%sedlex buf with
    | (hex_digit, hex_digit) ->
      let hex_digits = lexeme buf in
      let char_code = (int_of_hex_digit hex_digits.[0] * 16) + int_of_hex_digit hex_digits.[1] in
      Buffer.add_char builder (Char.chr char_code);
      iter lex
    | hex_digit -> err ()
    | _ -> err ()
  in
  iter lex

let tokenize lex =
  let open Token in
  let { buf; _ } = lex in
  let token_result token = Token (lex, { loc = current_loc lex; token }) in
  let lex_error err = LexError (lex, (current_loc lex, err)) in
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
  | '_' -> token_result T_WILDCARD
  | '.' -> token_result T_PERIOD
  | ',' -> token_result T_COMMA
  | '=' -> token_result T_EQUALS
  | '+' -> token_result T_PLUS
  | '-' -> token_result T_MINUS
  | '*' -> token_result T_MULTIPLY
  | '/' -> token_result T_DIVIDE
  | '%' -> token_result T_PERCENT
  | '!' -> token_result T_BANG
  | '&' -> token_result T_AMPERSAND
  | '^' -> token_result T_CARET
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
  | "match" -> token_result T_MATCH
  | "when" -> token_result T_WHEN
  | "module" -> token_result T_MODULE
  | "import" -> token_result T_IMPORT
  | "as" -> token_result T_AS
  | "is" -> token_result T_IS
  | "type" -> token_result T_TYPE
  | "alias" -> token_result T_ALIAS
  | "builtin" -> token_result T_BUILTIN
  | "trait" -> token_result T_TRAIT
  | "methods" -> token_result T_METHODS
  | "extends" -> token_result T_EXTENDS
  | "implements" -> token_result T_IMPLEMENTS
  | "static" -> token_result T_STATIC
  | "override" -> token_result T_OVERRIDE
  | "super" -> token_result T_SUPER
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
  | '"' -> parse_string_literal lex ~is_interpolated:false
  | '`' -> parse_string_literal lex ~is_interpolated:true
  | any -> lex_error (Parse_error.UnknownToken (lexeme buf))
  | _ -> sedlex_catch_all_case ()

let next lexer =
  let rec find_next_token lexer =
    match tokenize lexer with
    | Skip lexer -> find_next_token lexer
    | Token (lexer, result) -> (lexer, Ok result)
    | LexError (lexer, result) -> (lexer, Error result)
  in
  find_next_token lexer

let next_in_interpolated_string lexer =
  match parse_string_literal ~is_interpolated:true lexer with
  | Token (lexer, result) -> (lexer, Ok result)
  | LexError (lexer, result) -> (lexer, Error result)
  | Skip _ -> failwith "Skip cannot appear in interpolated string"
