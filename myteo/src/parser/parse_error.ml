type t =
  | UnknownToken of string
  | UnexpectedToken of {
      actual: Token.t;
      expected: Token.t option;
    }
  | MalformedTopLevel of Token.t
  | MalformedPattern of Token.t
  | MalformedFunctionBody of Token.t
  | MalformedType of Token.t
  | MissingModule of Token.t
  | InvalidAssignmentPattern

exception Fatal of (Loc.t * t)

let fatal err = raise (Fatal err)

let to_string error =
  match error with
  | UnknownToken raw -> Printf.sprintf "Unexpected token \"%s\"" raw
  | UnexpectedToken { actual = T_EOF; expected = None } -> "Unexpected <EOF>"
  | UnexpectedToken { actual; expected = None } ->
    Printf.sprintf "Unexpected token \"%s\"" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some (T_IDENTIFIER _) } ->
    Printf.sprintf "Unexpected token \"%s\", expected identifier" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some expected } ->
    Printf.sprintf
      "Unexpected token \"%s\", expected \"%s\""
      (Token.to_string actual)
      (Token.to_string expected)
  | MalformedTopLevel actual ->
    Printf.sprintf
      "Unexpected token \"%s\", expected start of top level statement"
      (Token.to_string actual)
  | MalformedPattern actual ->
    Printf.sprintf "Unexpected token \"%s\", expected start of pattern" (Token.to_string actual)
  | MalformedFunctionBody actual ->
    Printf.sprintf
      "Unexpected token \"%s\", expected start of function body"
      (Token.to_string actual)
  | MalformedType actual ->
    Printf.sprintf "Unexpected token \"%s\", expected start of type" (Token.to_string actual)
  | MissingModule actual ->
    Printf.sprintf
      "File must start with a module declaration. Found \"%s\" but expected \"module\""
      (Token.to_string actual)
  | InvalidAssignmentPattern -> "Left side of assignment must be a pattern"
