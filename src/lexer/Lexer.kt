package myte.lexer

import java.io.Reader

private class LL1StatefulReader(private val reader: Reader) {
	private var currentChar: Int
	private var nextChar: Int

	init {
		currentChar = reader.read()
		nextChar = reader.read()
	}

	val current: Char
		get() = if (currentChar != -1) currentChar.toChar() else throw LexicalException("Unexpected EOF")

	val next: Char
		get() = if (nextChar != -1) nextChar.toChar() else throw LexicalException("Unexpected EOF")

	val hasCurrent: Boolean
		get() = currentChar != -1

	val hasNext: Boolean
		get() = nextChar != -1

	fun advance() {
		currentChar = nextChar

		if (hasCurrent) {
			nextChar = reader.read()
		}
	}
}

private val keywordToTokenMap = mapOf(
	"true" to TrueToken,
	"false" to FalseToken,
	"let" to LetToken,
	"def" to DefToken,
	"num" to NumToken,
	"if" to IfToken,
	"else" to ElseToken,
	"while" to WhileToken,
	"do" to DoToken,
	"for" to ForToken,
	"return" to ReturnToken,
	"unit" to UnitToken,
	"bool" to BoolToken,
	"float" to FloatToken
)

private fun readNumberString(reader: LL1StatefulReader): String {
	var numberString = StringBuilder()

	// If readNumber is called, the current character must be numeric
	numberString.append(reader.current)

	// Read digits before decimal point
	while (reader.hasNext && reader.next in '0'..'9') {
		numberString.append(reader.next)
		reader.advance()
	}

	// Read decimal point
	if (reader.hasNext && reader.next == '.') {
		numberString.append(reader.next)
		reader.advance()
	}

	// Read digits after decimal point
	while (reader.hasNext && reader.next in '0'..'9') {
		numberString.append(reader.next)
		reader.advance()
	}

	// Read exponent
	if (reader.hasNext && (reader.next == 'e' || reader.next == 'E')) {
		numberString.append(reader.next)
		reader.advance()
	} else {
		return numberString.toString()
	}

	// Read exponent sign
	if (reader.hasNext && (reader.next == '+' || reader.next == '-')) {
		numberString.append(reader.next)
		reader.advance()
	} else {
		return numberString.toString()
	}

	if (reader.hasNext && reader.next !in '0'..'9') {
		throw LexicalException("Malformed number ${numberString.toString()}")
	}

	// Read exponent digits
	while (reader.hasNext && reader.next in '0'..'9') {
		numberString.append(reader.next)
		reader.advance()
	}

	return numberString.toString()
}

private fun readNumber(reader: LL1StatefulReader): NumberToken {
	val numberString = readNumberString(reader)

	try {
		return NumberToken(numberString.toDouble())
	} catch (e: NumberFormatException) {
		throw LexicalException("Malformed number ${numberString}")
	}
}

private fun readString(reader: LL1StatefulReader): Token {
	var str = StringBuilder()

	// If readString is called, the current character must be valid
	str.append(reader.current)

	while (reader.hasNext && reader.next.isValidIdentifierCharacter()) {
		str.append(reader.next)
		reader.advance()
	}

	// Check if string is a keyword
	var string = str.toString()
	var keywordToken = keywordToTokenMap[string]

	return keywordToken ?: StringToken(string)
}

private fun Char.isValidIdentifierCharacter(): Boolean = this.isLetterOrDigit() || this.equals('_')

private fun readEquals(reader: LL1StatefulReader): Token {
	// If readEquals is called, the current character must be '='
	if (reader.hasNext && reader.next == '=') {
		reader.advance()
		return DoubleEqualsToken
	} else {
		return EqualsToken
	}
}

private fun readLessThan(reader: LL1StatefulReader): Token {
	// If readLessThan is called, the current character must be '<'
	if (reader.hasNext && reader.next == '=') {
		reader.advance()
		return LessThanOrEqualToken
	} else {
		return LessThanToken
	}
}

private fun readGreaterThan(reader: LL1StatefulReader): Token {
	// If readGreaterThan is called, the current character must be '>'
	if (reader.hasNext && reader.next == '=') {
		reader.advance()
		return GreaterThanOrEqualToken
	} else {
		return GreaterThanToken
	}
}

private fun readBang(reader: LL1StatefulReader): Token {
	// If readBang is called, the current character must be "!"
	if (reader.hasNext && reader.next == '=') {
		reader.advance()
		return NotEqualsToken
	} else {
		return LogicalNotToken
	}
}

private fun readAmpersand(reader: LL1StatefulReader): LogicalAndToken {
	// If readAmpersand is called, the current character must be "&"
	if (reader.hasNext && reader.next == '&') {
		reader.advance()
		return LogicalAndToken
	} else {
		throw LexicalException("Unexpected token encountered &${reader.next}")
	}
}

private fun readPipe(reader: LL1StatefulReader): LogicalOrToken {
	// If readPipe is called, the current character must be "|"
	if (reader.hasNext && reader.next == '|') {
		reader.advance()
		return LogicalOrToken
	} else {
		throw LexicalException("Unexpected token encountered |${reader.next}")
	}
}

fun createTokens(input: Reader): List<Token> {
	val tokens: MutableList<Token> = mutableListOf()
	val reader = LL1StatefulReader(input)

	while (reader.hasCurrent) {
		when (reader.current) {
			'+' -> tokens.add(PlusToken)
			'-' -> tokens.add(MinusToken)
			'*' -> tokens.add(AsteriskToken)
			'/' -> tokens.add(ForwardSlashToken)
			'^' -> tokens.add(CaretToken)
			'=' -> tokens.add(readEquals(reader))
			'<' -> tokens.add(readLessThan(reader))
			'>' -> tokens.add(readGreaterThan(reader))
			'!' -> tokens.add(readBang(reader))
			'&' -> tokens.add(readAmpersand(reader))
			'|' -> tokens.add(readPipe(reader))
			'(' -> tokens.add(LeftParenToken)
			')' -> tokens.add(RightParenToken)
			'{' -> tokens.add(LeftBraceToken)
			'}' -> tokens.add(RightBraceToken)
			',' -> tokens.add(CommaToken)
			':' -> tokens.add(ColonToken)
			in '0'..'9' -> tokens.add(readNumber(reader))
			else -> {
				if (reader.current.isValidIdentifierCharacter()) {
					tokens.add(readString(reader))
				} else if (!reader.current.isWhitespace()) {
					throw LexicalException("Invalid character ${reader.current} in identifier, only "
						+ "alphanumeric characters or underscores are allowed.")
				}
			}
		}

		reader.advance()
	}

	return tokens
}
