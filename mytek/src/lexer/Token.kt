package myte.lexer

import java.math.BigDecimal
import java.math.BigInteger

import myte.shared.*

sealed class Token(val location: Location, val type: TokenType) {    
    override fun toString(): String = type.toString()
}

class IntegralLiteralToken(
    val num: BigInteger,
    location: Location
) : Token(location, TokenType.INTEGRAL_LITERAL) {
    override fun toString(): String = num.toString()
}

class DecimalLiteralToken(
    val num: BigDecimal,
    location: Location
) : Token(location, TokenType.DECIMAL_LITERAL) {
    override fun toString(): String = num.toString()
}

class StringLiteralToken(
    val str: String,
    location: Location
) : Token(location, TokenType.STRING_LITERAL) {
    override fun toString(): String = "\"${str}\""
}

open class IdentifierToken(
    val str: String,
    location: Location,
    type: TokenType = TokenType.IDENTIFIER
) : Token(location, type) {
    override fun toString(): String = str
}

class TrueToken(location: Location) : Token(location, TokenType.TRUE)

class FalseToken(location: Location) : Token(location, TokenType.FALSE)

class PlusToken(location: Location) : Token(location, TokenType.PLUS)

class MinusToken(location: Location) : Token(location, TokenType.MINUS)

class AsteriskToken(location: Location) : Token(location, TokenType.ASTERISK)

class ForwardSlashToken(location: Location) : Token(location, TokenType.FORWARD_SLASH)

class CaretToken(location: Location) : Token(location, TokenType.CARET)

class PercentToken(location: Location) : Token(location, TokenType.PERCENT)

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

class LeftMapLiteralToken(location: Location) : Token(location, TokenType.LEFT_MAP_LITERAL)

class RightMapLiteralToken(location: Location) : Token(location, TokenType.RIGHT_MAP_LITERAL)

class LeftSetLiteralToken(location: Location) : Token(location, TokenType.LEFT_SET_LITERAL)

class RightSetLiteralToken(location: Location) : Token(location, TokenType.RIGHT_SET_LITERAL)

class QuotesToken(location: Location) : Token(location, TokenType.QUOTES)

class PeriodToken(location: Location) : Token(location, TokenType.PERIOD)

class CommaToken(location: Location) : Token(location, TokenType.COMMA)

class ColonToken(location: Location) : Token(location, TokenType.COLON)

class ArrowToken(location: Location) : Token(location, TokenType.ARROW)

class PipeToken(location: Location) : Token(location, TokenType.PIPE)

class ScopeToken(location: Location) : Token(location, TokenType.SCOPE)

class TypeToken(location: Location) : Token(location, TokenType.TYPE)

class LetToken(location: Location) : Token(location, TokenType.LET)

class ConstToken(location: Location) : Token(location, TokenType.CONST)

class DefToken(location: Location) : Token(location, TokenType.DEF)

class FunToken(location: Location) : Token(location, TokenType.FUN)

class IfToken(location: Location) : Token(location, TokenType.IF)

class ElseToken(location: Location) : Token(location, TokenType.ELSE)

class WhileToken(location: Location) : Token(location, TokenType.WHILE)

class DoToken(location: Location) : Token(location, TokenType.DO)

class ForToken(location: Location) : Token(location, TokenType.FOR)

class ForEachToken(location: Location) : Token(location, TokenType.FOR_EACH)

class InToken(location: Location) : Token(location, TokenType.IN)

class MatchToken(location: Location) : Token(location, TokenType.MATCH)

class WhenToken(location: Location) : Token(location, TokenType.WHEN)

class ReturnToken(location: Location) : Token(location, TokenType.RETURN)

class BreakToken(location: Location) : Token(location, TokenType.BREAK)

class ContinueToken(location: Location) : Token(location, TokenType.CONTINUE)

class UnitToken(
    location: Location
) : IdentifierToken(TokenType.UNIT.toString(), location, TokenType.UNIT)

class BoolToken(
    location: Location
) : IdentifierToken(TokenType.BOOL.toString(), location, TokenType.BOOL)

class StringTypeToken(
    location: Location
) : IdentifierToken(TokenType.STRING_TYPE.toString(), location, TokenType.STRING_TYPE)

class ByteToken(
    location: Location
) : IdentifierToken(TokenType.BYTE.toString(), location, TokenType.BYTE)

class IntToken(
    location: Location
) : IdentifierToken(TokenType.INT.toString(), location, TokenType.INT)

class FloatToken(
    location: Location
) : IdentifierToken(TokenType.FLOAT.toString(), location, TokenType.FLOAT)

class DoubleToken(
    location: Location
) : IdentifierToken(TokenType.DOUBLE.toString(), location, TokenType.DOUBLE)

class VecToken(
    location: Location
) : IdentifierToken(TokenType.VEC.toString(), location, TokenType.VEC)

class MapToken(
    location: Location
) : IdentifierToken(TokenType.MAP.toString(), location, TokenType.MAP)

class SetToken(
    location: Location
) : IdentifierToken(TokenType.SET.toString(), location, TokenType.SET)

class PackageToken(
    location: Location
) : IdentifierToken(TokenType.PACKAGE.toString(), location, TokenType.PACKAGE)

class ImportToken(
    location: Location
) : IdentifierToken(TokenType.IMPORT.toString(), location, TokenType.IMPORT)

class AsToken(
    location: Location
) : IdentifierToken(TokenType.AS.toString(), location, TokenType.AS)

class ImplementToken(
    location: Location
) : IdentifierToken(TokenType.IMPLEMENT.toString(), location, TokenType.IMPLEMENT)

class ExtendsToken(
    location: Location
) : IdentifierToken(TokenType.EXTENDS.toString(), location, TokenType.EXTENDS)

class TraitToken(
    location: Location
) : IdentifierToken(TokenType.TRAIT.toString(), location, TokenType.TRAIT)

class SigToken(
    location: Location
) : IdentifierToken(TokenType.SIG.toString(), location, TokenType.SIG)

class StaticToken(
    location: Location
) : IdentifierToken(TokenType.STATIC.toString(), location, TokenType.STATIC)

class MutToken(
    location: Location
) : IdentifierToken(TokenType.MUT.toString(), location, TokenType.MUT)

class BuiltinToken(location: Location): Token(location, TokenType.BUILTIN)

class NewLineToken(location: Location) : Token(location, TokenType.NEW_LINE)
