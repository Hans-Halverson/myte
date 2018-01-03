package myte.lexer

import myte.shared.*

sealed class Token(charNum: Int, lineNum: Int) {
    val context = Context(charNum, lineNum)
    
    abstract val type: TokenType
    override fun toString(): String = type.toString()
}

class IntLiteralToken(val num: Int, charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.INT_LITERAL
    override fun toString(): String = num.toString()
}

class FloatLiteralToken(
    val num: Double,
    charNum: Int,
    lineNum: Int
) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.FLOAT_LITERAL
    override fun toString(): String = num.toString()
}

class StringLiteralToken(
    val str: String,
    charNum: Int,
    lineNum: Int
) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.STRING_LITERAL
    override fun toString(): String = "\"${str}\""
}

class IdentifierToken(val str: String, charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.IDENTIFIER
    override fun toString(): String = str
}

class TrueToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.TRUE
}

class FalseToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.FALSE
}

class PlusToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.PLUS
}

class MinusToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.MINUS
}

class AsteriskToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.ASTERISK
}

class ForwardSlashToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.FORWARD_SLASH
}

class CaretToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.CARET
}

class EqualsToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.EQUALS
}

class DoubleEqualsToken(charNum: Int, lineNum: Int): Token(charNum, lineNum) {
    override val type: TokenType = TokenType.DOUBLE_EQUALS
}

class LessThanToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LESS_THAN
}

class LessThanOrEqualToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LESS_THAN_OR_EQUAL
}

class GreaterThanToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.GREATER_THAN
}

class GreaterThanOrEqualToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.GREATER_THAN_OR_EQUAL
}

class NotEqualsToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.NOT_EQUALS
}

class LogicalNotToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LOGICAL_NOT
}

class LogicalAndToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LOGICAL_AND
}

class LogicalOrToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LOGICAL_OR
}

class LeftParenToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LEFT_PAREN
}

class RightParenToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.RIGHT_PAREN
}

class LeftBraceToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LEFT_BRACE
}

class RightBraceToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.RIGHT_BRACE
}

class LeftBracketToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LEFT_BRACKET
}

class RightBracketToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.RIGHT_BRACKET
}

class QuotesToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.QUOTES
}

class CommaToken(charNum: Int, lineNum: Int): Token(charNum, lineNum) {
    override val type: TokenType = TokenType.COMMA
}

class ColonToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.COLON
}

class ArrowToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.ARROW
}

class PipeToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.PIPE
}

class TypeToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.TYPE
}

class OfToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.OF
}

class LetToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.LET
}

class ConstToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.CONST
}

class DefToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.DEF
}

class NumToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.NUM
}

class IfToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.IF
}

class ElseToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.ELSE
}

class WhileToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.WHILE
}

class DoToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.DO
}

class ForToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.FOR
}

class MatchToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.MATCH
}

class ReturnToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.RETURN
}

class BreakToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.BREAK
}

class ContinueToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.CONTINUE
}

class UnitToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.UNIT
}

class BoolToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.BOOL
}

class StringTypeToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.STRING_TYPE
}

class IntToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.INT
}

class FloatToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.FLOAT
}

class VecToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.VEC
}

class NewLineToken(charNum: Int, lineNum: Int) : Token(charNum, lineNum) {
    override val type: TokenType = TokenType.NEW_LINE
}
