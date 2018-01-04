package myte.lexer

import myte.shared.*

sealed class Token(val context: Context) {    
    abstract val type: TokenType
    override fun toString(): String = type.toString()
}

class IntLiteralToken(val num: Int, context: Context) : Token(context) {
    override val type: TokenType = TokenType.INT_LITERAL
    override fun toString(): String = num.toString()
}

class FloatLiteralToken(val num: Double, context: Context) : Token(context) {
    override val type: TokenType = TokenType.FLOAT_LITERAL
    override fun toString(): String = num.toString()
}

class StringLiteralToken(val str: String, context: Context) : Token(context) {
    override val type: TokenType = TokenType.STRING_LITERAL
    override fun toString(): String = "\"${str}\""
}

class IdentifierToken(val str: String, context: Context) : Token(context) {
    override val type: TokenType = TokenType.IDENTIFIER
    override fun toString(): String = str
}

class TrueToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.TRUE
}

class FalseToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.FALSE
}

class PlusToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.PLUS
}

class MinusToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.MINUS
}

class AsteriskToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.ASTERISK
}

class ForwardSlashToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.FORWARD_SLASH
}

class CaretToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.CARET
}

class EqualsToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.EQUALS
}

class DoubleEqualsToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.DOUBLE_EQUALS
}

class LessThanToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LESS_THAN
}

class LessThanOrEqualToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LESS_THAN_OR_EQUAL
}

class GreaterThanToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.GREATER_THAN
}

class GreaterThanOrEqualToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.GREATER_THAN_OR_EQUAL
}

class NotEqualsToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.NOT_EQUALS
}

class LogicalNotToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LOGICAL_NOT
}

class LogicalAndToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LOGICAL_AND
}

class LogicalOrToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LOGICAL_OR
}

class LeftParenToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LEFT_PAREN
}

class RightParenToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.RIGHT_PAREN
}

class LeftBraceToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LEFT_BRACE
}

class RightBraceToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.RIGHT_BRACE
}

class LeftBracketToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LEFT_BRACKET
}

class RightBracketToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.RIGHT_BRACKET
}

class QuotesToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.QUOTES
}

class CommaToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.COMMA
}

class ColonToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.COLON
}

class ArrowToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.ARROW
}

class PipeToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.PIPE
}

class TypeToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.TYPE
}

class LetToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.LET
}

class ConstToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.CONST
}

class DefToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.DEF
}

class NumToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.NUM
}

class IfToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.IF
}

class ElseToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.ELSE
}

class WhileToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.WHILE
}

class DoToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.DO
}

class ForToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.FOR
}

class MatchToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.MATCH
}

class ReturnToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.RETURN
}

class BreakToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.BREAK
}

class ContinueToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.CONTINUE
}

class UnitToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.UNIT
}

class BoolToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.BOOL
}

class StringTypeToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.STRING_TYPE
}

class IntToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.INT
}

class FloatToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.FLOAT
}

class VecToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.VEC
}

class NewLineToken(context: Context) : Token(context) {
    override val type: TokenType = TokenType.NEW_LINE
}
