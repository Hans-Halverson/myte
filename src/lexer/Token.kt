package myte.lexer

import myte.shared.*

sealed class Token {
    abstract val type: TokenType
}

data class IntLiteralToken(val num: Int) : Token() {
    override val type: TokenType = TokenType.INT_LITERAL
}

data class FloatLiteralToken(val num: Double) : Token() {
    override val type: TokenType = TokenType.FLOAT_LITERAL
}

data class StringLiteralToken(val str: String) : Token() {
    override val type: TokenType = TokenType.STRING_LITERAL
}

data class IdentifierToken(val str: String) : Token() {
    override val type: TokenType = TokenType.IDENTIFIER
}

object TrueToken : Token() {
    override val type: TokenType = TokenType.TRUE
    override fun toString(): String = "true"
}

object FalseToken : Token() {
    override val type: TokenType = TokenType.FALSE
    override fun toString(): String = "false"
}

object PlusToken : Token() {
    override val type: TokenType = TokenType.PLUS
    override fun toString(): String = "+"
}

object MinusToken : Token() {
    override val type: TokenType = TokenType.MINUS
    override fun toString(): String = "-"
}

object AsteriskToken : Token() {
    override val type: TokenType = TokenType.ASTERISK
    override fun toString(): String = "*"
}

object ForwardSlashToken : Token() {
    override val type: TokenType = TokenType.FORWARD_SLASH
    override fun toString(): String = "/"
}

object CaretToken : Token() {
    override val type: TokenType = TokenType.CARET
    override fun toString(): String = "^"
}

object EqualsToken: Token() {
    override val type: TokenType = TokenType.EQUALS
    override fun toString(): String = "="
}

object DoubleEqualsToken: Token() {
    override val type: TokenType = TokenType.DOUBLE_EQUALS
    override fun toString(): String = "=="
}

object LessThanToken: Token() {
    override val type: TokenType = TokenType.LESS_THAN
    override fun toString(): String = "<"
}

object LessThanOrEqualToken: Token() {
    override val type: TokenType = TokenType.LESS_THAN_OR_EQUAL
    override fun toString(): String = "<="
}

object GreaterThanToken: Token() {
    override val type: TokenType = TokenType.GREATER_THAN
    override fun toString(): String = ">"
}

object GreaterThanOrEqualToken: Token() {
    override val type: TokenType = TokenType.GREATER_THAN_OR_EQUAL
    override fun toString(): String = ">="
}

object NotEqualsToken: Token() {
    override val type: TokenType = TokenType.NOT_EQUALS
    override fun toString(): String = "!="
}

object LogicalNotToken: Token() {
    override val type: TokenType = TokenType.LOGICAL_NOT
    override fun toString(): String = "!"
}

object LogicalAndToken: Token() {
    override val type: TokenType = TokenType.LOGICAL_AND
    override fun toString(): String = "&&"
}

object LogicalOrToken: Token() {
    override val type: TokenType = TokenType.LOGICAL_OR
    override fun toString(): String = "||"
}

object LeftParenToken : Token() {
    override val type: TokenType = TokenType.LEFT_PAREN
    override fun toString(): String = "("
}

object RightParenToken : Token() {
    override val type: TokenType = TokenType.RIGHT_PAREN
    override fun toString(): String = ")"
}

object LeftBraceToken : Token() {
    override val type: TokenType = TokenType.LEFT_BRACE
    override fun toString(): String = "{"
}

object RightBraceToken : Token() {
    override val type: TokenType = TokenType.RIGHT_BRACE
    override fun toString(): String = "}"
}

object LeftBracketToken : Token() {
    override val type: TokenType = TokenType.LEFT_BRACKET
    override fun toString(): String = "["
}

object RightBracketToken : Token() {
    override val type: TokenType = TokenType.RIGHT_BRACKET
    override fun toString(): String = "]"
}

object QuotesToken: Token() {
    override val type: TokenType = TokenType.QUOTES
    override fun toString(): String = "\""
}

object CommaToken: Token() {
    override val type: TokenType = TokenType.COMMA
    override fun toString(): String = ","
}

object ColonToken: Token() {
    override val type: TokenType = TokenType.COLON
    override fun toString(): String = ":"
}

object ArrowToken: Token() {
    override val type: TokenType = TokenType.ARROW
    override fun toString(): String = "->"
}

object PipeToken: Token() {
    override val type: TokenType = TokenType.PIPE
    override fun toString(): String = "|"
}

object TypeToken: Token() {
    override val type: TokenType = TokenType.TYPE
    override fun toString(): String = "type"
}

object OfToken: Token() {
    override val type: TokenType = TokenType.OF
    override fun toString(): String = "of"
}

object LetToken: Token() {
    override val type: TokenType = TokenType.LET
    override fun toString(): String = "let"
}

object ConstToken: Token() {
    override val type: TokenType = TokenType.CONST
    override fun toString(): String = "const"
}

object DefToken: Token() {
    override val type: TokenType = TokenType.DEF
    override fun toString(): String = "def"
}

object NumToken: Token() {
    override val type: TokenType = TokenType.NUM
    override fun toString(): String = "num"
}

object IfToken: Token() {
    override val type: TokenType = TokenType.IF
    override fun toString(): String = "if"
}

object ElseToken: Token() {
    override val type: TokenType = TokenType.ELSE
    override fun toString(): String = "else"
}

object WhileToken: Token() {
    override val type: TokenType = TokenType.WHILE
    override fun toString(): String = "while"
}

object DoToken: Token() {
    override val type: TokenType = TokenType.DO
    override fun toString(): String = "do"
}

object ForToken: Token() {
    override val type: TokenType = TokenType.FOR
    override fun toString(): String = "for"
}

object MatchToken: Token() {
    override val type: TokenType = TokenType.MATCH
    override fun toString(): String = "match"
}

object ReturnToken: Token() {
    override val type: TokenType = TokenType.RETURN
    override fun toString(): String = "return"
}

object BreakToken: Token() {
    override val type: TokenType = TokenType.BREAK
    override fun toString(): String = "break"
}

object ContinueToken: Token() {
    override val type: TokenType = TokenType.CONTINUE
    override fun toString(): String = "continue"
}

object UnitToken: Token() {
    override val type: TokenType = TokenType.UNIT
    override fun toString(): String = "unit"
}

object BoolToken: Token() {
    override val type: TokenType = TokenType.BOOL
    override fun toString(): String = "bool"
}

object StringTypeToken: Token() {
    override val type: TokenType = TokenType.STRING_TYPE
    override fun toString(): String = "string"
}

object IntToken: Token() {
    override val type: TokenType = TokenType.INT
    override fun toString(): String = "int"
}

object FloatToken: Token() {
    override val type: TokenType = TokenType.FLOAT
    override fun toString(): String = "float"
}

object VecToken: Token() {
    override val type: TokenType = TokenType.VEC
    override fun toString(): String = "vec"
}

object NewLineToken: Token() {
    override val type: TokenType = TokenType.NEW_LINE
    override fun toString(): String = "<NewLine>"
}
