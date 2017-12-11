package myte.lexer

import myte.shared.*

sealed class Token {
	abstract val type: TokenType
}

data class NumberToken(val num: Double) : Token() {
	override val type: TokenType = TokenType.NUMBER
}

data class StringToken(val str: String) : Token() {
	override val type: TokenType = TokenType.STRING
}

class TrueToken() : Token() {
	override val type: TokenType = TokenType.TRUE
	override fun toString(): String = "true"
}

class FalseToken() : Token() {
	override val type: TokenType = TokenType.FALSE
	override fun toString(): String = "false"
}

class PlusToken : Token() {
	override val type: TokenType = TokenType.PLUS
	override fun toString(): String = "+"
}

class MinusToken : Token() {
	override val type: TokenType = TokenType.MINUS
	override fun toString(): String = "-"
}

class AsteriskToken : Token() {
	override val type: TokenType = TokenType.ASTERISK
	override fun toString(): String = "*"
}

class ForwardSlashToken : Token() {
	override val type: TokenType = TokenType.FORWARD_SLASH
	override fun toString(): String = "/"
}

class CaretToken : Token() {
	override val type: TokenType = TokenType.CARET
	override fun toString(): String = "^"
}

class EqualsToken: Token() {
	override val type: TokenType = TokenType.EQUALS
	override fun toString(): String = "="
}

class DoubleEqualsToken: Token() {
	override val type: TokenType = TokenType.DOUBLE_EQUALS
	override fun toString(): String = "=="
}

class LessThanToken: Token() {
	override val type: TokenType = TokenType.LESS_THAN
	override fun toString(): String = "<"
}

class LessThanOrEqualToken: Token() {
	override val type: TokenType = TokenType.LESS_THAN_OR_EQUAL
	override fun toString(): String = "<="
}

class GreaterThanToken: Token() {
	override val type: TokenType = TokenType.GREATER_THAN
	override fun toString(): String = ">"
}

class GreaterThanOrEqualToken: Token() {
	override val type: TokenType = TokenType.GREATER_THAN_OR_EQUAL
	override fun toString(): String = ">="
}

class NotEqualsToken: Token() {
	override val type: TokenType = TokenType.NOT_EQUALS
	override fun toString(): String = "!="
}

class LogicalNotToken: Token() {
	override val type: TokenType = TokenType.LOGICAL_NOT
	override fun toString(): String = "!"
}

class LogicalAndToken: Token() {
	override val type: TokenType = TokenType.LOGICAL_AND
	override fun toString(): String = "&&"
}

class LogicalOrToken: Token() {
	override val type: TokenType = TokenType.LOGICAL_OR
	override fun toString(): String = "||"
}

class LeftParenToken : Token() {
	override val type: TokenType = TokenType.LEFT_PAREN
	override fun toString(): String = "("
}

class RightParenToken : Token() {
	override val type: TokenType = TokenType.RIGHT_PAREN
	override fun toString(): String = ")"
}

class LeftBraceToken : Token() {
	override val type: TokenType = TokenType.LEFT_BRACE
	override fun toString(): String = "{"
}

class RightBraceToken : Token() {
	override val type: TokenType = TokenType.RIGHT_BRACE
	override fun toString(): String = "}"
}

class CommaToken: Token() {
	override val type: TokenType = TokenType.COMMA
	override fun toString(): String = ","
}

class ColonToken: Token() {
	override val type: TokenType = TokenType.COLON
	override fun toString(): String = ":"
}

class LetToken: Token() {
	override val type: TokenType = TokenType.LET
	override fun toString(): String = "let"
}

class DefToken: Token() {
	override val type: TokenType = TokenType.DEF
	override fun toString(): String = "def"
}

class NumToken: Token() {
	override val type: TokenType = TokenType.NUM
	override fun toString(): String = "num"
}

class IfToken: Token() {
	override val type: TokenType = TokenType.IF
	override fun toString(): String = "if"
}

class ElseToken: Token() {
	override val type: TokenType = TokenType.ELSE
	override fun toString(): String = "else"
}

class WhileToken: Token() {
	override val type: TokenType = TokenType.WHILE
	override fun toString(): String = "while"
}

class DoToken: Token() {
	override val type: TokenType = TokenType.DO
	override fun toString(): String = "do"
}

class ForToken: Token() {
	override val type: TokenType = TokenType.FOR
	override fun toString(): String = "for"
}

class NewLineToken: Token() {
	override val type: TokenType = TokenType.NEW_LINE
	override fun toString(): String = "<NewLine>"
}
