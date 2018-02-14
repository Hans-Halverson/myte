package myte.lexer

import myte.shared.*

sealed class Token(val context: Context, val type: TokenType) {    
    override fun toString(): String = type.toString()
}

class IntLiteralToken(val num: Int, context: Context) : Token(context, TokenType.INT_LITERAL) {
    override fun toString(): String = num.toString()
}

class FloatLiteralToken(
    val num: Double,
    context: Context
) : Token(context, TokenType.FLOAT_LITERAL) {
    override fun toString(): String = num.toString()
}

class StringLiteralToken(
    val str: String,
    context: Context
) : Token(context, TokenType.STRING_LITERAL) {
    override fun toString(): String = "\"${str}\""
}

class IdentifierToken(val str: String, context: Context) : Token(context, TokenType.IDENTIFIER) {
    override fun toString(): String = str
}

class TrueToken(context: Context) : Token(context, TokenType.TRUE)

class FalseToken(context: Context) : Token(context, TokenType.FALSE)

class PlusToken(context: Context) : Token(context, TokenType.PLUS)

class MinusToken(context: Context) : Token(context, TokenType.MINUS)

class AsteriskToken(context: Context) : Token(context, TokenType.ASTERISK)

class ForwardSlashToken(context: Context) : Token(context, TokenType.FORWARD_SLASH)

class CaretToken(context: Context) : Token(context, TokenType.CARET)

class EqualsToken(context: Context) : Token(context, TokenType.EQUALS)

class DoubleEqualsToken(context: Context) : Token(context, TokenType.DOUBLE_EQUALS)

class LessThanToken(context: Context) : Token(context, TokenType.LESS_THAN)

class LessThanOrEqualToken(context: Context) : Token(context, TokenType.LESS_THAN_OR_EQUAL)

class GreaterThanToken(context: Context) : Token(context, TokenType.GREATER_THAN)

class GreaterThanOrEqualToken(context: Context) : Token(context, TokenType.GREATER_THAN_OR_EQUAL)

class NotEqualsToken(context: Context) : Token(context, TokenType.NOT_EQUALS)

class LogicalNotToken(context: Context) : Token(context, TokenType.LOGICAL_NOT)

class LogicalAndToken(context: Context) : Token(context, TokenType.LOGICAL_AND)

class LogicalOrToken(context: Context) : Token(context, TokenType.LOGICAL_OR)

class LeftParenToken(context: Context) : Token(context, TokenType.LEFT_PAREN)

class RightParenToken(context: Context) : Token(context, TokenType.RIGHT_PAREN)

class LeftBraceToken(context: Context) : Token(context, TokenType.LEFT_BRACE)

class RightBraceToken(context: Context) : Token(context, TokenType.RIGHT_BRACE)

class LeftBracketToken(context: Context) : Token(context, TokenType.LEFT_BRACKET)

class RightBracketToken(context: Context) : Token(context, TokenType.RIGHT_BRACKET)

class QuotesToken(context: Context) : Token(context, TokenType.QUOTES)

class PeriodToken(context: Context) : Token(context, TokenType.PERIOD)

class CommaToken(context: Context) : Token(context, TokenType.COMMA)

class ColonToken(context: Context) : Token(context, TokenType.COLON)

class ArrowToken(context: Context) : Token(context, TokenType.ARROW)

class PipeToken(context: Context) : Token(context, TokenType.PIPE)

class TypeToken(context: Context) : Token(context, TokenType.TYPE)


class LetToken(context: Context) : Token(context, TokenType.LET)

class ConstToken(context: Context) : Token(context, TokenType.CONST)

class DefToken(context: Context) : Token(context, TokenType.DEF)

class NumToken(context: Context) : Token(context, TokenType.NUM)


class IfToken(context: Context) : Token(context, TokenType.IF)

class ElseToken(context: Context) : Token(context, TokenType.ELSE)

class WhileToken(context: Context) : Token(context, TokenType.WHILE)

class DoToken(context: Context) : Token(context, TokenType.DO)

class ForToken(context: Context) : Token(context, TokenType.FOR)

class MatchToken(context: Context) : Token(context, TokenType.MATCH)

class ReturnToken(context: Context) : Token(context, TokenType.RETURN)

class BreakToken(context: Context) : Token(context, TokenType.BREAK)

class ContinueToken(context: Context) : Token(context, TokenType.CONTINUE)

class UnitToken(context: Context) : Token(context, TokenType.UNIT)

class BoolToken(context: Context) : Token(context, TokenType.BOOL)

class StringTypeToken(context: Context) : Token(context, TokenType.STRING_TYPE)

class IntToken(context: Context) : Token(context, TokenType.INT)

class FloatToken(context: Context) : Token(context, TokenType.FLOAT)

class VecToken(context: Context) : Token(context, TokenType.VEC)

class NewLineToken(context: Context) : Token(context, TokenType.NEW_LINE)
