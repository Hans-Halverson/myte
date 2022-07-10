type t =
  | UnknownToken of string
  | UnexpectedToken of {
      actual: Token.t;
      expected: Token.t option;
    }
  | UnexpectedTokens of Token.t * Token.t list
  | UnterminatedStringLiteral
  | InvalidEscape of bool
  | InvalidHexEscape
  | InvalidCharEscape
  | MalformedFloatLiteral
  | MalformedCharLiteral
  | MalformedTopLevel of Token.t
  | MalformedMethodsItem of Token.t
  | MalformedPattern of Token.t
  | MalformedFunctionBody of Token.t
  | MalformedTypeDeclaration of Token.t
  | MalformedType of Token.t
  | MissingModule of Token.t
  | InvalidAssignmentPattern
  | LiteralInPattern
  | EmptyRecord
  | EmptyTuple
  | SingleVariant
  | InvalidBuiltin
  | FileDoesNotExist of string
  | FileIsDirectory of string
  | CannotOpenFile of string

exception Fatal of (Loc.t * t)

let fatal err = raise (Fatal err)

let to_string error =
  match error with
  | UnknownToken raw -> Printf.sprintf "Unexpected token `%s`" raw
  | UnexpectedToken { actual = T_EOF; expected = None } -> "Unexpected <EOF>"
  | UnexpectedToken { actual; expected = None } ->
    Printf.sprintf "Unexpected token `%s`" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some (T_IDENTIFIER _) } ->
    Printf.sprintf "Unexpected token `%s`, expected identifier" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some expected } ->
    Printf.sprintf
      "Unexpected token `%s`, expected `%s`"
      (Token.to_string actual)
      (Token.to_string expected)
  | UnexpectedTokens (actual, expecteds) ->
    Printf.sprintf
      "Unexpected token `%s`, expected %s"
      (Token.to_string actual)
      (Error_utils.concat_with_or (List.map (fun s -> "`" ^ Token.to_string s ^ "`") expecteds))
  | UnterminatedStringLiteral -> "Unterminated string literal"
  | InvalidEscape is_interpolated ->
    let escapes =
      if is_interpolated then
        "```, `$`, `\\`, `n`, `t`, `r`, or `x`"
      else
        "`\"`, `\\`, `n`, `t`, `r`, or `x`"
    in
    Printf.sprintf "Invalid escape sequence, expected %s" escapes
  | InvalidHexEscape ->
    "Invalid hex escape sequence, expected exactly two hex digits following `\\x`"
  | InvalidCharEscape -> "Invalid character escape sequence, expected `'`, `\\`, `n`, `t`, or `r`"
  | MalformedFloatLiteral -> "Malformed float literal, exponent must contain at least one digit"
  | MalformedCharLiteral -> "Malformed character literal"
  | MalformedTopLevel actual ->
    Printf.sprintf
      "Unexpected token `%s`, expected start of top level declaration"
      (Token.to_string actual)
  | MalformedMethodsItem actual ->
    Printf.sprintf "Unexpected token `%s`, expected start of method" (Token.to_string actual)
  | MalformedPattern actual ->
    Printf.sprintf "Unexpected token `%s`, expected start of pattern" (Token.to_string actual)
  | MalformedFunctionBody actual ->
    Printf.sprintf "Unexpected token `%s`, expected start of function body" (Token.to_string actual)
  | MalformedTypeDeclaration actual ->
    Printf.sprintf
      "Unexpected token `%s`, expected start of type declaration"
      (Token.to_string actual)
  | MalformedType actual ->
    Printf.sprintf "Unexpected token `%s`, expected start of type" (Token.to_string actual)
  | MissingModule actual ->
    Printf.sprintf
      "File must start with a module declaration. Found `%s` but expected `module`."
      (Token.to_string actual)
  | InvalidAssignmentPattern -> "Invalid left hand side of assignment"
  | LiteralInPattern -> "Literals cannot appear in variable declaration patterns"
  | EmptyRecord -> "Record must have at least one field"
  | EmptyTuple -> "Tuple must have at least one element"
  | SingleVariant -> "Variant type must have at least two variants"
  | InvalidBuiltin ->
    "A type alias cannot be builtin. Only function and type declarations can be builtin."
  | FileDoesNotExist file -> Printf.sprintf "Cannot find file %s" file
  | FileIsDirectory file -> Printf.sprintf "File %s is a directory" file
  | CannotOpenFile file -> Printf.sprintf "Cannot open file %s" file
