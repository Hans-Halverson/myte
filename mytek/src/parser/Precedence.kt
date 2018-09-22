package myte.parser

import myte.lexer.*

/* 
 * Precedence levels ascend from 0, a higher precedence binds expressions more tightly, and will be
 * applied before operators of a lower precedence.
 */

// Precedence levels for expression
const val EXPR_NO_PRECEDENCE: Int = 0
const val EXPR_ASSIGNMENT_PRECEDENCE: Int = 1
const val EXPR_LOGICAL_OR_PRECEDENCE: Int = 2
const val EXPR_LOGICAL_AND_PRECEDENCE: Int = 3
const val EXPR_LOGICAL_NOT_PRECEDENCE: Int = 4
const val EXPR_COMPARISON_PRECEDENCE: Int = 5
const val EXPR_ADD_PRECEDENCE: Int = 6
const val EXPR_MULTIPLY_PRECEDENCE: Int = 7
const val EXPR_EXPONENT_PRECEDENCE: Int = 8
const val EXPR_NUMERIC_PREFIX_PRECEDENCE: Int = 9
const val EXPR_APPLICATION_OR_ACCESS_PRECEDENCE: Int = 10

// Precedence levels for type expressions
const val TYPE_NO_PRECEDENCE: Int = 0
const val TYPE_FUNCTION_PRECEDENCE: Int = 1

/**
 * Given a token found between two expressions, assign a precedence level to the operator or
 * synatic structure that was found.
 */
fun getExprPrecedenceForInfixToken(tokenType: TokenType): Int = when (tokenType) {
    TokenType.EQUALS -> EXPR_ASSIGNMENT_PRECEDENCE
    TokenType.LOGICAL_OR -> EXPR_LOGICAL_OR_PRECEDENCE
    TokenType.LOGICAL_AND -> EXPR_LOGICAL_AND_PRECEDENCE
    TokenType.DOUBLE_EQUALS -> EXPR_COMPARISON_PRECEDENCE
    TokenType.NOT_EQUALS -> EXPR_COMPARISON_PRECEDENCE
    TokenType.LESS_THAN -> EXPR_COMPARISON_PRECEDENCE
    TokenType.LESS_THAN_OR_EQUAL -> EXPR_COMPARISON_PRECEDENCE
    TokenType.GREATER_THAN -> EXPR_COMPARISON_PRECEDENCE
    TokenType.GREATER_THAN_OR_EQUAL -> EXPR_COMPARISON_PRECEDENCE
    TokenType.PLUS -> EXPR_ADD_PRECEDENCE
    TokenType.MINUS -> EXPR_ADD_PRECEDENCE
    TokenType.ASTERISK -> EXPR_MULTIPLY_PRECEDENCE
    TokenType.FORWARD_SLASH -> EXPR_MULTIPLY_PRECEDENCE
    TokenType.PERCENT -> EXPR_MULTIPLY_PRECEDENCE
    TokenType.CARET -> EXPR_EXPONENT_PRECEDENCE
    TokenType.PERIOD -> EXPR_APPLICATION_OR_ACCESS_PRECEDENCE
    TokenType.LEFT_BRACKET -> EXPR_APPLICATION_OR_ACCESS_PRECEDENCE
    TokenType.LEFT_PAREN -> EXPR_APPLICATION_OR_ACCESS_PRECEDENCE
    TokenType.LEFT_BRACE -> EXPR_APPLICATION_OR_ACCESS_PRECEDENCE
    else -> EXPR_NO_PRECEDENCE
}

/**
 * Given a token found between two type expressions, assign a precedence level to the type
 * type constructor structure that was found.
 */
fun getTypePrecedenceForInfixToken(tokenType: TokenType): Int = when (tokenType) {
    TokenType.ARROW -> TYPE_FUNCTION_PRECEDENCE
    else -> TYPE_NO_PRECEDENCE
}

/**
 * Make a precedence right associative by subtracting one from it.
 */
fun rightAssociative(precedence: Int): Int = precedence - 1
