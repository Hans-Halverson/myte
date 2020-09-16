type t =
  | T_IDENTIFIER of string
  | T_INT_LITERAL of int * string
  | T_STRING_LITERAL of string
  | T_BOOL_LITERAL of bool
  | T_SEMICOLON
  | T_COLON
  | T_COMMA
  | T_PERIOD
  | T_ARROW
  | T_BIG_ARROW
  | T_PLUS
  | T_MINUS
  | T_MULTIPLY
  | T_DIVIDE
  | T_LOGICAL_AND
  | T_LOGICAL_OR
  | T_LOGICAL_NOT
  | T_EQUALS
  | T_DOUBLE_EQUALS
  | T_NOT_EQUALS
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LESS_THAN_OR_EQUAL
  | T_GREATER_THAN_OR_EQUAL
  | T_LEFT_PAREN
  | T_RIGHT_PAREN
  | T_LEFT_BRACE
  | T_RIGHT_BRACE
  | T_VAR
  | T_VAL
  | T_FUN
  | T_IF
  | T_ELSE
  | T_RETURN
  | T_MODULE
  | T_IMPORT
  | T_AS
  | T_TYPE
  | T_UNIT
  | T_INT
  | T_STRING
  | T_BOOL
  | T_EOF

let to_string token =
  match token with
  | T_IDENTIFIER name -> name
  | T_INT_LITERAL (_, raw) -> raw
  | T_STRING_LITERAL value -> "\"" ^ value ^ "\""
  | T_BOOL_LITERAL value ->
    if value then
      "true"
    else
      "false"
  | T_SEMICOLON -> ";"
  | T_COLON -> ":"
  | T_COMMA -> ","
  | T_PERIOD -> "."
  | T_ARROW -> "->"
  | T_BIG_ARROW -> "=>"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_MULTIPLY -> "*"
  | T_DIVIDE -> "/"
  | T_LOGICAL_AND -> "&&"
  | T_LOGICAL_OR -> "||"
  | T_LOGICAL_NOT -> "!"
  | T_EQUALS -> "="
  | T_DOUBLE_EQUALS -> "=="
  | T_NOT_EQUALS -> "!="
  | T_LESS_THAN -> "<"
  | T_GREATER_THAN -> ">"
  | T_LESS_THAN_OR_EQUAL -> "<="
  | T_GREATER_THAN_OR_EQUAL -> ">="
  | T_LEFT_PAREN -> "("
  | T_RIGHT_PAREN -> ")"
  | T_LEFT_BRACE -> "{"
  | T_RIGHT_BRACE -> "}"
  | T_VAR -> "var"
  | T_VAL -> "val"
  | T_FUN -> "fun"
  | T_IF -> "if"
  | T_ELSE -> "else"
  | T_RETURN -> "return"
  | T_MODULE -> "module"
  | T_IMPORT -> "import"
  | T_AS -> "as"
  | T_TYPE -> "type"
  | T_UNIT -> "unit"
  | T_INT -> "int"
  | T_STRING -> "string"
  | T_BOOL -> "bool"
  | T_EOF -> "<EOF>"
