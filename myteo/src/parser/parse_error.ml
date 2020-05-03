type t =
  | UnknownToken of string
  | UnexpectedToken of {
      actual: Token.t;
      expected: Token.t option;
    }
  | MalformedFunctionBody of Token.t
  | MalformedType of Token.t

exception Fatal of (Loc.t * t)

let fatal err = raise (Fatal err)

let to_string error =
  match error with
  | UnknownToken raw -> Printf.sprintf "Unexpected token \"%s\"" raw
  | UnexpectedToken { actual = T_EOF; expected = None } -> "Unexpected <EOF>"
  | UnexpectedToken { actual; expected = None } ->
    Printf.sprintf "Unexpected token \"%s\"" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some expected } ->
    Printf.sprintf
      "Unexpected token \"%s\", expected \"%s\""
      (Token.to_string actual)
      (Token.to_string expected)
  | MalformedFunctionBody actual ->
    Printf.sprintf
      "Unexpected token \"%s\", expected start of function body"
      (Token.to_string actual)
  | MalformedType actual ->
    Printf.sprintf "Unexpected token \"%s\", expected start of type" (Token.to_string actual)
