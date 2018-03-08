package myte.lexer

import myte.shared.*

sealed class Token(val location: Location, val type: TokenType) {    
    override fun toString(): String = type.toString()
}

class IntLiteralToken(val num: Int, location: Location) : Token(location, TokenType.INT_LITERAL) {
    override fun toString(): String = num.toString()
}

class FloatLiteralToken(
    val num: Double,
    location: Location
) : Token(location, TokenType.FLOAT_LITERAL) {
    override fun toString(): String = num.toString()
}

class StringLiteralToken(
    val str: String,
    location: Location
) : Token(location, TokenType.STRING_LITERAL) {
    override fun toString(): String = "\"${str}\""
}

class IdentifierToken(val str: String, location: Location) : Token(location, TokenType.IDENTIFIER) {
    override fun toString(): String = str
}

class TrueToken(location: Location) : Token(location, TokenType.TRUE)

class FalseToken(location: Location) : Token(location, TokenType.FALSE)

class PlusToken(location: Location) : Token(location, TokenType.PLUS)

class MinusToken(location: Location) : Token(location, TokenType.MINUS)

class AsteriskToken(location: Location) : Token(location, TokenType.ASTERISK)

class ForwardSlashToken(location: Location) : Token(location, TokenType.FORWARD_SLASH)

class CaretToken(location: Location) : Token(location, TokenType.CARET)

class EqualsToken(location: Location) : Token(location, TokenType.EQUALS)

class DoubleEqualsToken(location: Location) : Token(location, TokenType.DOUBLE_EQUALS)

class LessThanToken(location: Location) : Token(location, TokenType.LESS_THAN)

class LessThanOrEqualToken(location: Location) : Token(location, TokenType.LESS_THAN_OR_EQUAL)

class GreaterThanToken(location: Location) : Token(location, TokenType.GREATER_THAN)

class GreaterThanOrEqualToken(location: Location) : Token(location, TokenType.GREATER_THAN_OR_EQUAL)

class NotEqualsToken(location: Location) : Token(location, TokenType.NOT_EQUALS)

class LogicalNotToken(location: Location) : Token(location, TokenType.LOGICAL_NOT)

class LogicalAndToken(location: Location) : Token(location, TokenType.LOGICAL_AND)

class LogicalOrToken(location: Location) : Token(location, TokenType.LOGICAL_OR)

class LeftParenToken(location: Location) : Token(location, TokenType.LEFT_PAREN)

class RightParenToken(location: Location) : Token(location, TokenType.RIGHT_PAREN)

class LeftBraceToken(location: Location) : Token(location, TokenType.LEFT_BRACE)

class RightBraceToken(location: Location) : Token(location, TokenType.RIGHT_BRACE)

class LeftBracketToken(location: Location) : Token(location, TokenType.LEFT_BRACKET)

class RightBracketToken(location: Location) : Token(location, TokenType.RIGHT_BRACKET)

class QuotesToken(location: Location) : Token(location, TokenType.QUOTES)

class PeriodToken(location: Location) : Token(location, TokenType.PERIOD)

class CommaToken(location: Location) : Token(location, TokenType.COMMA)

class ColonToken(location: Location) : Token(location, TokenType.COLON)

class ArrowToken(location: Location) : Token(location, TokenType.ARROW)

class PipeToken(location: Location) : Token(location, TokenType.PIPE)

class TypeToken(location: Location) : Token(location, TokenType.TYPE)


class LetToken(location: Location) : Token(location, TokenType.LET)

class ConstToken(location: Location) : Token(location, TokenType.CONST)

class DefToken(location: Location) : Token(location, TokenType.DEF)

class NumToken(location: Location) : Token(location, TokenType.NUM)


class IfToken(location: Location) : Token(location, TokenType.IF)

class ElseToken(location: Location) : Token(location, TokenType.ELSE)

class WhileToken(location: Location) : Token(location, TokenType.WHILE)

class DoToken(location: Location) : Token(location, TokenType.DO)

class ForToken(location: Location) : Token(location, TokenType.FOR)

class MatchToken(location: Location) : Token(location, TokenType.MATCH)

class ReturnToken(location: Location) : Token(location, TokenType.RETURN)

class BreakToken(location: Location) : Token(location, TokenType.BREAK)

class ContinueToken(location: Location) : Token(location, TokenType.CONTINUE)

class UnitToken(location: Location) : Token(location, TokenType.UNIT)

class BoolToken(location: Location) : Token(location, TokenType.BOOL)

class StringTypeToken(location: Location) : Token(location, TokenType.STRING_TYPE)

class IntToken(location: Location) : Token(location, TokenType.INT)

class FloatToken(location: Location) : Token(location, TokenType.FLOAT)

class VecToken(location: Location) : Token(location, TokenType.VEC)

class NewLineToken(location: Location) : Token(location, TokenType.NEW_LINE)
