type t =
  | T_IDENTIFIER of string
  | T_INT_LITERAL of string * Integers.base
  | T_FLOAT_LITERAL of string
  | T_CHAR_LITERAL of char
  | T_STRING_LITERAL of string
  | T_BOOL_LITERAL of bool
  | T_INTERPOLATED_STRING of string * (* Is end *) bool
  | T_SEMICOLON
  | T_COLON
  | T_QUESTION
  | T_PIPE
  | T_COMMA
  | T_PERIOD
  | T_ARROW
  | T_PLUS
  | T_MINUS
  | T_MULTIPLY
  | T_DIVIDE
  | T_PERCENT
  | T_LOGICAL_AND
  | T_LOGICAL_OR
  | T_WILDCARD
  | T_AT
  | T_BANG
  | T_EQUALS
  | T_DOUBLE_EQUALS
  | T_NOT_EQUALS
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LESS_THAN_OR_EQUAL
  | T_GREATER_THAN_OR_EQUAL
  | T_AMPERSAND
  | T_CARET
  | T_PLUS_EQUALS
  | T_MINUS_EQUALS
  | T_MULTIPLY_EQUALS
  | T_DIVIDE_EQUALS
  | T_PERCENT_EQUALS
  | T_AMPERSAND_EQUALS
  | T_PIPE_EQUALS
  | T_CARET_EQUALS
  | T_LEFT_SHIFT_EQUALS
  | T_ARITHMETIC_RIGHT_SHIFT_EQUALS
  | T_LOGICAL_RIGHT_SHIFT_EQUALS
  | T_LEFT_PAREN
  | T_RIGHT_PAREN
  | T_LEFT_BRACE
  | T_RIGHT_BRACE
  | T_LEFT_BRACKET
  | T_RIGHT_BRACKET
  | T_SET_OPEN
  | T_SET_CLOSE
  | T_VAR
  | T_VAL
  | T_FUN
  | T_FN
  | T_IF
  | T_ELSE
  | T_WHILE
  | T_FOR
  | T_IN
  | T_RETURN
  | T_BREAK
  | T_CONTINUE
  | T_MATCH
  | T_WHEN
  | T_MODULE
  | T_IMPORT
  | T_AS
  | T_TYPE
  | T_ALIAS
  | T_TRAIT
  | T_METHODS
  | T_EXTENDS
  | T_IMPLEMENTS
  | T_PUB
  | T_STATIC
  | T_OVERRIDE
  | T_EOF

let to_string token =
  match token with
  | T_IDENTIFIER name -> name
  | T_INT_LITERAL (raw, _) -> raw
  | T_FLOAT_LITERAL raw -> raw
  | T_CHAR_LITERAL value -> Integers.char_to_string value
  | T_STRING_LITERAL value -> "\"" ^ value ^ "\""
  | T_BOOL_LITERAL value ->
    if value then
      "true"
    else
      "false"
  | T_INTERPOLATED_STRING _ -> "<interpolated string>"
  | T_SEMICOLON -> ";"
  | T_COLON -> ":"
  | T_QUESTION -> "?"
  | T_PIPE -> "|"
  | T_COMMA -> ","
  | T_PERIOD -> "."
  | T_ARROW -> "->"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_MULTIPLY -> "*"
  | T_DIVIDE -> "/"
  | T_PERCENT -> "%"
  | T_LOGICAL_AND -> "&&"
  | T_LOGICAL_OR -> "||"
  | T_WILDCARD -> "_"
  | T_AT -> "@"
  | T_BANG -> "!"
  | T_EQUALS -> "="
  | T_DOUBLE_EQUALS -> "=="
  | T_NOT_EQUALS -> "!="
  | T_LESS_THAN -> "<"
  | T_GREATER_THAN -> ">"
  | T_LESS_THAN_OR_EQUAL -> "<="
  | T_GREATER_THAN_OR_EQUAL -> ">="
  | T_AMPERSAND -> "&"
  | T_CARET -> "^"
  | T_PLUS_EQUALS -> "+="
  | T_MINUS_EQUALS -> "-="
  | T_MULTIPLY_EQUALS -> "*="
  | T_DIVIDE_EQUALS -> "/="
  | T_PERCENT_EQUALS -> "%="
  | T_AMPERSAND_EQUALS -> "&="
  | T_PIPE_EQUALS -> "|="
  | T_CARET_EQUALS -> "^="
  | T_LEFT_SHIFT_EQUALS -> "<<="
  | T_ARITHMETIC_RIGHT_SHIFT_EQUALS -> ">>="
  | T_LOGICAL_RIGHT_SHIFT_EQUALS -> ">>>="
  | T_LEFT_PAREN -> "("
  | T_RIGHT_PAREN -> ")"
  | T_LEFT_BRACE -> "{"
  | T_RIGHT_BRACE -> "}"
  | T_LEFT_BRACKET -> "["
  | T_RIGHT_BRACKET -> "]"
  | T_SET_OPEN -> "{|"
  | T_SET_CLOSE -> "|}"
  | T_VAR -> "var"
  | T_VAL -> "val"
  | T_FUN -> "fun"
  | T_FN -> "fn"
  | T_IF -> "if"
  | T_ELSE -> "else"
  | T_WHILE -> "while"
  | T_FOR -> "for"
  | T_IN -> "in"
  | T_RETURN -> "return"
  | T_BREAK -> "break"
  | T_CONTINUE -> "continue"
  | T_MATCH -> "match"
  | T_WHEN -> "when"
  | T_MODULE -> "module"
  | T_IMPORT -> "import"
  | T_AS -> "as"
  | T_TYPE -> "type"
  | T_ALIAS -> "alias"
  | T_TRAIT -> "trait"
  | T_METHODS -> "methods"
  | T_EXTENDS -> "extends"
  | T_IMPLEMENTS -> "implements"
  | T_PUB -> "pub"
  | T_STATIC -> "static"
  | T_OVERRIDE -> "override"
  | T_EOF -> "<EOF>"
