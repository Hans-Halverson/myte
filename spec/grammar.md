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
      | SCOPED_IDENT ( EXPR [, EXPR]* )
      | SCOPED_IDENT { IDENT : EXPR [, IDENT : EXPR]* }
      | UNARY
      | INFIX
      | ( [EXPR_LIST]? )
      | EXPR ( [EXPR_LIST]? )
      | EXPR [ EXPR ]
      | fun ( [OPT_TYPED_ARGS]? ) -> LAMBDA_BODY
      | BLOCK
      | IF_STATEMENT
      | MATCH_STATEMENT
      | EXPR . IDENT

UNARY -> + EXPR
       | - EXPR
       | ! EXPR

INFIX -> EXPR + EXPR
       | EXPR - EXPR
       | EXPR * EXPR
       | EXPR / EXPR
       | EXPR ^ EXPR
       | EXPR % EXPR
       | EXPR == EXPR
       | EXPR != EXPR
       | EXPR < EXPR
       | EXPR > EXPR
       | EXPR <= EXPR
       | EXPR >= EXPR
       | EXPR && EXPR
       | EXPR || EXPR
       | LVALUE = EXPR

EXPR_LIST -> EXPR [, EXPR]*

KV_PAIR -> EXPR -> EXPR

KV_PAIR_LIST -> KV_PAIR [, KV_PAIR]*

LAMBDA_BODY -> EXPR
             | BLOCK

LVALUE -> IDENT
        | IDENT.IDENT
        | ( LVALUE [, LVALUE]* )
        | SCOPED_IDENT ( LVALUE [, LVALUE]* )


TYPE -> bool
      | int
      | float
      | unit
      | string
      | ( TYPE_LIST )
      | TYPE -> TYPE
      | vec < TYPE >
      | map < TYPE, TYPE >
      | set < TYPE >
      | SCOPED_IDENT [< TYPE_LIST >]?

TYPE_LIST -> TYPE [, TYPE]*

OPT_TYPED_IDENT = IDENT [: TYPE]?

TYPED_IDENT = IDENT : TYPE

OPT_TYPED_ARGS -> OPT_TYPED_IDENT [, OPT_TYPED_IDENT]*

TYPED_ARGS -> TYPED_IDENT [, TYPED_IDENT]*

VARIABLE_DEF -> let LVALUE [: TYPE]? = EXPR
              | const LVALUE [: TYPE]? = EXPR

FUNCTION_DEF -> def IDENT ( [TYPED_ARGS]? ) [: TYPE]? FUNCTION_BODY

FUNCTION_BODY -> BLOCK
               | = EXPR


TYPE_DEF -> type IDENT [< TYPE_PARAM_LIST >]? = [| TYPE_VARIANT]+

TYPE_PARAM_LIST -> IDENT [, IDENT]*

TYPE_VARIANT -> IDENT
              | IDENT ( TYPE_LIST )
              | IDENT { [mut]? IDENT : TYPE [, IDENT : TYPE]* }


IF_STATEMENT -> if ( EXPR ) STATEMENT [else STATEMENT]?

WHILE_STATEMENT -> while ( EXPR ) STATEMENT

DO_WHILE_STATEMENT -> do STATEMENT while ( EXPR )

FOR_STATEMENT -> for ( STATEMENT? , EXPR? , STATEMENT? ) STATEMENT

FOR_EACH_STATEMENT -> forEach ( LVALUE [: TYPE]? in EXPR ) STATEMENT

RETURN_STATEMENT -> return unit
                  | return EXPR

MATCH_STATEMENT -> match EXPR [| PATTERN [when EXPR]? -> STATEMENT]+

PATTERN -> NUMBER_LITERAL
      | BOOL_LITERAL
      | " STRING_LITERAL "
      | [ [PATTERN_LIST]? ]
      | ( PATTERN_LIST )
      | IDENT
      | SCOPED_IDENT
      | SCOPED_IDENT ( PATTERN_LIST )
      | SCOPED_IDENT { IDENT : PATTERN [, IDENT : PATTERN]* }

PATTERN_LIST = PATTERN [, PATTERN]*


STATEMENT -> EXPR
           | BLOCK
           | FUNCTION_DEF
           | VARIABLE_DEF
           | IF_STATEMENT
           | WHILE_STATEMENT
           | DO_WHILE_STATEMENT
           | FOR_STATEMENT
           | FOR_EACH_STATEMENT
           | MATCH_STATEMENT
           | RETURN_STATEMENT
           | continue
           | break

BLOCK -> { STATEMENT* }


SCOPED_IDENT_PARAMS -> SCOPED_IDENT [< TYPE_PARAM_LIST >]?

TRAIT_LIST -> SCOPED_IDENT_PARAMS [, SCOPED_IDENT_PARAMS]*

TYPE_IMPL -> implement SCOPED_IDENT_PARAMS [extends TRAIT_LIST]? { [TYPE_FUNCTION]* }

TYPE_FUNCTION -> [static]? FUNCTION_DEF

TRAIT_DEF -> trait IDENT [< TYPE_PARAM_LIST >]? [extends TRAIT_LIST]? { [TRAIT_FUNCTION]* }

TRAIT_FUNCTION -> [static]? sig IDENT ( TYPE_LIST ) [: TYPE]?
                | [static]? FUNCTION_DEF


TOP_LEVEL_STATEMENT -> TYPE_DEF
                     | TYPE_IMPL
                     | FUNCTION_DEF
                     | VARIABLE_DEF


PACKAGE -> package PACKAGE_NAME

IMPORT -> import PACKAGE_NAME [as IDENT]?
IMPORT -> import PACKAGE_NAME :: { IDENT [as IDENT]? [, IDENT [as IDENT]?]* }

REPL_LINE -> STATEMENT
           | TYPE_DEF
           | TYPE_IMPL
           | IMPORT

FILE -> PACKAGE [IMPORT]* [TOP_LEVEL_STATEMENT]*

