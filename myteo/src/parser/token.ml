type t =
  | T_IDENTIFIER of string
  | T_INT_LITERAL of int * string
  | T_STRING_LITERAL of string
  | T_BOOL_LITERAL of bool
  | T_SEMICOLON
  | T_PLUS
  | T_MINUS
  | T_MULTIPLY
  | T_DIVIDE
  | T_EOF

let to_string token =
  match token with
  | T_IDENTIFIER name -> name
  | T_INT_LITERAL (_, raw) -> raw
  | T_STRING_LITERAL value -> "\"" ^ value ^ "\""
  | T_BOOL_LITERAL value -> if value then "true" else "false"
  | T_SEMICOLON -> ";"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_MULTIPLY -> "*"
  | T_DIVIDE -> "/"
  | T_EOF -> "<EOF>"