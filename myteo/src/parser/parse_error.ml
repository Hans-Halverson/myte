type t =
  | UnknownToken of string
  | UnexpectedToken of { actual: Token.t; expected: Token.t option }

exception Fatal of t

let fatal err = raise (Fatal err)

let to_string error =
  match error with
  | UnknownToken raw -> Printf.sprintf "Unexpected token %s" raw
  | UnexpectedToken { actual; expected = None } ->
    Printf.sprintf "Unexpected token %s" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some expected } ->
    Printf.sprintf
      "Unexpected token %s, expected %s"
      (Token.to_string actual)
      (Token.to_string expected)

let print (loc, error) =
  let source =
    match loc with
    | { Loc.file = Some file; _ } -> Printf.sprintf "%s:" file
    | _ -> ""
  in
  Printf.sprintf "%s%d:%d error: %s" source Loc.(loc.start.line) Loc.(loc.start.col) (to_string error)