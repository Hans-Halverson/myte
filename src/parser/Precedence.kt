package myte.parser

import myte.lexer.*

// Precedence levels ascend from 0, a higher precedence binds expressions more tightly, and will be
// applied before operators of a lower precedence.
const val NO_PRECEDENCE: Int = 0
const val KEYED_ACCESS_PRECEDENCE = 1
const val ASSIGNMENT_PRECEDENCE: Int = 2
const val LOGICAL_OR_PRECEDENCE: Int = 3
const val LOGICAL_AND_PRECEDENCE: Int = 4
const val LOGICAL_NOT_PRECEDENCE: Int = 5
const val COMPARISON_PRECEDENCE: Int = 6
const val ADD_PRECEDENCE: Int = 7
const val MULTIPLY_PRECEDENCE: Int = 8
const val EXPONENT_PRECEDENCE: Int = 9
const val NUMERIC_PREFIX_PRECEDENCE: Int = 10
const val CALL_PRECEDENCE: Int = 11

/**
 * Given a token found between two expressions, assign a precedence level to the operator or
 * synatic structure that was found.
 */
fun getPrecedenceForInfixToken(tokenType: TokenType): Int = when (tokenType) {
    TokenType.LEFT_BRACKET -> KEYED_ACCESS_PRECEDENCE
    TokenType.EQUALS -> ASSIGNMENT_PRECEDENCE
    TokenType.LOGICAL_OR -> LOGICAL_OR_PRECEDENCE
    TokenType.LOGICAL_AND -> LOGICAL_AND_PRECEDENCE
    TokenType.DOUBLE_EQUALS -> COMPARISON_PRECEDENCE
    TokenType.NOT_EQUALS -> COMPARISON_PRECEDENCE
    TokenType.LESS_THAN -> COMPARISON_PRECEDENCE
    TokenType.LESS_THAN_OR_EQUAL -> COMPARISON_PRECEDENCE
    TokenType.GREATER_THAN -> COMPARISON_PRECEDENCE
    TokenType.GREATER_THAN_OR_EQUAL -> COMPARISON_PRECEDENCE
    TokenType.PLUS -> ADD_PRECEDENCE
    TokenType.MINUS -> ADD_PRECEDENCE
    TokenType.ASTERISK -> MULTIPLY_PRECEDENCE
    TokenType.FORWARD_SLASH -> MULTIPLY_PRECEDENCE
    TokenType.CARET -> EXPONENT_PRECEDENCE
    TokenType.LEFT_PAREN -> CALL_PRECEDENCE
    else -> NO_PRECEDENCE
}

/**
 * Make a precedence right associative by subtracting one from it.
 */
fun rightAssociative(precedence: Int): Int = precedence - 1
