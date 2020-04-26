type t =
  | T_IDENTIFIER of string
  | T_SEMICOLON
  | T_EOF

let to_string token =
  match token with
  | T_IDENTIFIER name -> name
  | T_SEMICOLON -> ";"
  | T_EOF -> "<EOF>"