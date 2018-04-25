# Identifiers
Identifiers must consists of only alphanumeric characters and underscores, and must begin with an alphabetic character.

# Full grammar

SCOPED_IDENT -> IDENT [:: IDENT]*

EXPR -> NUMBER_LITERAL
      | BOOL_LITERAL
      | " STRING_LITERAL "
      | [ EXPR_LIST ]
      | [| KV_PAIR_LIST |]
      | {| EXPR_LIST |}
      | SCOPED_IDENT
      | UNARY
      | INFIX
      | ( [EXPR_LIST]? )
      | EXPR ( [EXPR_LIST]? )
      | EXPR [ EXPR ]
      | fun ( [TYPED_ARGS]? ) -> LAMBDA_BODY

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

EXPR_LIST -> EXPR [, EXPR]*

KV_PAIR -> EXPR -> EXPR

KV_PAIR_LIST -> KV_PAIR [, KV_PAIR]*

LAMBDA_BODY -> EXPR
             | BLOCK


TYPE -> bool
      | float
      | unit
      | ( TYPE_LIST )
      | TYPE -> TYPE
      | TYPE | TYPE
      | vec < TYPE >
      | map < TYPE, TYPE >
      | set < TYPE >
      | SCOPED_IDENT [< TYPE_LIST >]?

TYPE_LIST -> TYPE [, TYPE]*


TYPED_IDENT = IDENT [: TYPE]?

VARIABLE_DEF -> let TYPED_IDENT = EXPR
              | const TYPED_IDENT = EXPR

FUNCTION_DEF -> def IDENT ( [TYPED_ARGS]? ) [: TYPE]? FUNCTION_BODY

FUNCTION_BODY -> BLOCK
               | = EXPR

TYPED_ARGS -> TYPED_IDENT [, TYPED_IDENT]*


TYPE_DEF -> type IDENT [< TYPE_PARAM_LIST >]? = [| TYPE_VARIANT]+

TYPE_PARAM_LIST -> IDENT [, IDENT]*

TYPE_VARIANT -> IDENT [( TYPE_LIST )]?


IF_STATEMENT -> if EXPR STATEMENT [else STATEMENT]?

WHILE_STATEMENT -> while EXPR STATEMENT

DO_WHILE_STATEMENT -> do STATEMENT while EXPR

FOR_STATEMENT -> for ( STATEMENT? , EXPR? , STATEMENT? ) STATEMENT

RETURN_STATEMENT -> return unit
                  | return EXPR

MATCH_STATEMENT -> match EXPR [| PATTERN [when EXPR]? -> STATEMENT]+

PATTERN -> NUMBER_LITERAL
      | BOOL_LITERAL
      | " STRING_LITERAL "
      | [ [PATTERN_LIST]? ]
      | ( PATTERN_LIST )
      | IDENT
      | SCOPED_IDENT [( PATTERN_LIST )]?

PATTERN_LIST = PATTERN [, PATTERN]*


STATEMENT -> EXPR
           | BLOCK
           | FUNCTION_DEF
           | VARIABLE_DEF
           | IF_STATEMENT
           | WHILE_STATEMENT
           | DO_WHILE_STATEMENT
           | FOR_STATEMENT
           | MATCH_STATEMENT
           | RETURN_STATEMENT

BLOCK -> { STATEMENT* }



TOP_LEVEL_STATEMENT -> TYPE_DEF
                     | FUNCTION_DEF
                     | VARIABLE_DEF


PACKAGE -> package PACKAGE_NAME

IMPORT -> import PACKAGE_NAME [as IDENT]?

REPL_LINE -> STATEMENT
           | TYPE_DEF
           | IMPORT

FILE -> PACKAGE [IMPORT]* [TOP_LEVEL_STATEMENT]*

