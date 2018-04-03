# Identifiers
Identifiers must consists of only alphanumeric characters and underscores, and must begin with an alphabetic character.


# Arithmetic operations
The following two argument, infix arithmetic operators are defined: +, -, /, *.


# Order of operations
Operators have the following precedence, from low to high:
1. Addition and subtraction (+,-)
2. Multiplication and division (*,/)
3. Exponentiation (^)
4. Function application (f(x))


# Numerics
Values and functions defined with let are numeric, and can only interact with other numbers and numerics. General, non-numeric values and functions are defined with the var and def keywords, respectively.


# Number conversions
Implicit conversions from int to float are allowed when using arithmetic operators (+, -, /, *, ^) and comparisons (<, <=, >, >=), but not when function arguments.


# Full grammar

EXPR -> NUMBER_LITERAL
      | BOOL_LITERAL
      | " STRING_LITERAL "
      | [ EXPR_LIST ]
      | IDENT
      | UNARY
      | INFIX
      | ( EXPR_LIST )
      | FUNCTION_CALL
      | KEYED_ACCESS

UNARY -> + EXPR
       | - EXPR
       | ! EXPR

INFIX -> EXPR + EXPR
       | EXPR - EXPR
       | EXPR * EXPR
       | EXPR / EXPR
       | EXPR ^ EXPR
       | EXPR == EXPR
       | EXPR != EXPR
       | EXPR < EXPR
       | EXPR > EXPR
       | EXPR <= EXPR
       | EXPR >= EXPR
       | EXPR && EXPR
       | EXPR || EXPR
       | EXPR = EXPR

FUNCTION_CALL -> IDENT ( [EXPR_LIST]? )

KEYED_ACCESS -> IDENT [ EXPR ]

EXPR_LIST -> EXPR [, EXPR]*


TYPE -> bool
      | float
      | unit
      | ( TYPE_LIST )
      | TYPE -> TYPE
      | TYPE | TYPE
      | vec < TYPE_LIST >
      | IDENT [< TYPE_LIST >]?

TYPE_LIST -> TYPE [, TYPE]*


TYPED_IDENT = IDENT [: TYPE]?

VARIABLE_DEF -> let TYPED_IDENT = EXPR
              | let num IDENT = EXPR
              | const TYPED_IDENT = EXPR
              | const num IDENT = EXPR

FUNCTION_DEF -> def IDENT ( [TYPED_ARGS]? ) [: TYPE]? FUNCTION_BODY
              | def num IDENT ( [UNTYPED_ARGS]? ) FUNCTION_BODY

FUNCTION_BODY -> BLOCK
               | = EXPR

TYPED_ARGS -> TYPED_IDENT [, TYPED_IDENT]*

UNTYPED_ARGS -> IDENT [, IDENT]*


TYPE_DEF -> type IDENT [< TYPE_PARAM_LIST >]? = { TYPE_VARIANT_LIST }

UNION_TYPE_DEF -> union type IDENT [< TYPE_PARAM_LIST >]? = { UNION_TYPE_LIST }

TYPE_PARAM_LIST -> IDENT [, IDENT]*

TYPE_VARIANT_LIST -> TYPE_VARIANT [| TYPE_VARIANT_LIST]*

TYPE_VARIANT -> IDENT [( TYPE_LIST )]?

UNION_TYPE_LIST -> TYPE [| UNION_TYPE_LIST]*


IF_STATEMENT -> if EXPR STATEMENT [else STATEMENT]?

WHILE_STATEMENT -> while EXPR STATEMENT

DO_WHILE_STATEMENT -> do STATEMENT while EXPR

FOR_STATEMENT -> for ( STATEMENT? , EXPR? , STATEMENT? ) STATEMENT

RETURN_STATEMENT -> return unit
                  | return EXPR

MATCH_STATEMENT -> match EXPR { [MATCH_CASE]+ }

MATCH_CASE -> | PATTERN -> STATEMENT

PATTERN -> NUMBER_LITERAL
      | BOOL_LITERAL
      | " STRING_LITERAL "
      | [ [PATTERN_LIST]? ]
      | ( PATTERN_LIST )
      | IDENT
      | IDENT ( PATTERN_LIST )

PATTERN_LIST = PATTERN [, PATTERN]*


STATEMENT -> EXPR
           | BLOCK
           | FUNCTION_DEF
           | VARIABLE_DEF
           | IF_STATEMENT
           | WHILE_STATEMENT
           | FOR_STATEMENT
           | RETURN_STATEMENT

BLOCK -> { STATEMENT* }


REPL_LINE = STATEMENT
FILE = STATEMENT*

