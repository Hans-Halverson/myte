package myte.lexer

import java.io.Reader

/**
 * A reader which allows the user to inspect the current and next character from an input stream.
 * 
 * @property reader a reader that supplies the input stream
 */
private class LL1StatefulReader(private val reader: Reader) {
    private var currentChar: Int
    private var nextChar: Int

    var charNum: Int = 1
    var lineNum: Int = 1

    // Prime the reader the initally be set to look at the first and second characters
    init {
        currentChar = reader.read()
        nextChar = reader.read()
    }

    /**
     * Get the current character pointed to by the reader.
     * @throws LexicalException if the end of the stream has been reached
     */
    val current: Char
        get() {
            if (currentChar != -1) {
                return currentChar.toChar()
            } else {
                throw LexicalException("Unexpected EOF", charNum, lineNum)
            }
        }

    /**
     * Get the next character pointed to by the reader.
     * @throws LexicalException if the end of the stream has been reached
     */
    val next: Char
        get() {
            if (nextChar != -1) {
                return nextChar.toChar()
            } else {
                throw LexicalException("Unexpected EOF", charNum, lineNum)
            }
        }

    /**
     * Returns whether there is a token currently pointed to by the reader.
     */
    val hasCurrent: Boolean
        get() = currentChar != -1

    /**
     * Returns whether there is a token after the one currently pointed to by the reader.
     */
    val hasNext: Boolean
        get() = nextChar != -1

    /**
     * Move the reader forward one token in the stream.
     */
    fun advance() {
        // Increment character and line index according to current, consumed character.
        // Tabs count as 4 characters, all other characters count as a single character
        if (currentChar.toChar() == '\t') {
            charNum += 4
        } else if (currentChar.toChar() == '\n') {
            lineNum++
            charNum = 1
        } else {
            charNum++
        }

        currentChar = nextChar

        if (hasCurrent) {
            nextChar = reader.read()
        }
    }
}

/**
 * Given a string (along with the current character and line number), return a keyword token
 * for that string if the string is a keyword, otherwise return null.
 */
private fun getKeywordToken(str: String, charNum: Int, lineNum: Int): Token? {
    return when (str) {
        "true" -> TrueToken(charNum, lineNum)
        "false" -> FalseToken(charNum, lineNum)
        "type" -> TypeToken(charNum, lineNum)
        "of" -> OfToken(charNum, lineNum)
        "let" -> LetToken(charNum, lineNum)
        "const" -> ConstToken(charNum, lineNum)
        "def" -> DefToken(charNum, lineNum)
        "num" -> NumToken(charNum, lineNum)
        "if" -> IfToken(charNum, lineNum)
        "else" -> ElseToken(charNum, lineNum)
        "while" -> WhileToken(charNum, lineNum)
        "do" -> DoToken(charNum, lineNum)
        "for" -> ForToken(charNum, lineNum)
        "match" -> MatchToken(charNum, lineNum)
        "return" -> ReturnToken(charNum, lineNum)
        "break" -> BreakToken(charNum, lineNum)
        "continue" -> ContinueToken(charNum, lineNum)
        "unit" -> UnitToken(charNum, lineNum)
        "bool" -> BoolToken(charNum, lineNum)
        "string" -> StringTypeToken(charNum, lineNum)
        "int" -> IntToken(charNum, lineNum)
        "float" -> FloatToken(charNum, lineNum)
        "vec" -> VecToken(charNum, lineNum)
        else -> null
    }
}

///////////////////////////////////////////////////////////////////////////
// 
// Tokenizer states depending on what character was been read.
//
///////////////////////////////////////////////////////////////////////////

/**
 * Read a single number token from input stream, is read as either an int or float.
 * @throws LexicalException if the number is malformed
 */
private fun readNumber(reader: LL1StatefulReader): Token {
    var numberString = StringBuilder()

    val charNum = reader.charNum
    val lineNum = reader.lineNum

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
    } else {
        return stringToInt(numberString.toString(), charNum, lineNum)
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
        return stringToFloat(numberString.toString(), charNum, lineNum)
    }

    // Read exponent sign
    if (reader.hasNext && (reader.next == '+' || reader.next == '-')) {
        numberString.append(reader.next)
        reader.advance()
    } else {
        return stringToFloat(numberString.toString(), charNum, lineNum)
    }

    if (reader.hasNext && reader.next !in '0'..'9') {
        throw LexicalException("Malformed float ${numberString.toString()}", charNum, lineNum)
    }

    // Read exponent digits
    while (reader.hasNext && reader.next in '0'..'9') {
        numberString.append(reader.next)
        reader.advance()
    }

    return stringToFloat(numberString.toString(), charNum, lineNum)
}

/**
 * Convert a string to an int token.
 * @throws LexicalException if the string does not represent a valid int
 */
private fun stringToInt(str: String, charNum: Int, lineNum: Int): IntLiteralToken {
    try {
        return IntLiteralToken(str.toInt(), charNum, lineNum)
    } catch (e: NumberFormatException) {
        throw LexicalException("Malformed int ${str}", charNum, lineNum)
    }
}

/**
 * Convert a string to a float token.
 * @throws LexicalException if the string does not represent a valid float
 */
private fun stringToFloat(str: String, charNum: Int, lineNum: Int): FloatLiteralToken {
    try {
        return FloatLiteralToken(str.toDouble(), charNum, lineNum)
    } catch (e: NumberFormatException) {
        throw LexicalException("Malformed float ${str}", charNum, lineNum)
    }
}

/** 
 * Read a single string into either an identifier token, or a keyword token if the string is
 * a keyword in the keywords map.
 */
private fun readString(reader: LL1StatefulReader): Token {
    var str = StringBuilder()

    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readString is called, the current character must be valid
    str.append(reader.current)

    while (reader.hasNext && reader.next.isValidIdentifierCharacter()) {
        str.append(reader.next)
        reader.advance()
    }

    // Check if string is a keyword
    var string = str.toString()
    var keywordToken = getKeywordToken(string, charNum, lineNum)

    return keywordToken ?: IdentifierToken(string, charNum, lineNum)
}

// Identifiers in myte consist of only alphanumeric characters and underscores.
private fun Char.isValidIdentifierCharacter(): Boolean = this.isLetterOrDigit() || this.equals('_')

private fun readMinus(reader: LL1StatefulReader): Token {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readMinus is called, the current character must be "-"
    if (reader.hasNext && reader.next == '>') {
        reader.advance()
        return ArrowToken(charNum, lineNum)
    } else {
        return MinusToken(charNum, lineNum)
    }
}

private fun readEquals(reader: LL1StatefulReader): Token {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readEquals is called, the current character must be '='
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return DoubleEqualsToken(charNum, lineNum)
    } else {
        return EqualsToken(charNum, lineNum)
    }
}

private fun readLessThan(reader: LL1StatefulReader): Token {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readLessThan is called, the current character must be '<'
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return LessThanOrEqualToken(charNum, lineNum)
    } else {
        return LessThanToken(charNum, lineNum)
    }
}

private fun readGreaterThan(reader: LL1StatefulReader): Token {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readGreaterThan is called, the current character must be '>'
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return GreaterThanOrEqualToken(charNum, lineNum)
    } else {
        return GreaterThanToken(charNum, lineNum)
    }
}

private fun readBang(reader: LL1StatefulReader): Token {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readBang is called, the current character must be "!"
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return NotEqualsToken(charNum, lineNum)
    } else {
        return LogicalNotToken(charNum, lineNum)
    }
}

private fun readAmpersand(reader: LL1StatefulReader): LogicalAndToken {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readAmpersand is called, the current character must be "&"
    if (reader.hasNext && reader.next == '&') {
        reader.advance()
        return LogicalAndToken(charNum, lineNum)
    } else {
        throw LexicalException("Unexpected token encountered &${reader.next}", charNum, lineNum)
    }
}

private fun readPipe(reader: LL1StatefulReader): Token {
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    // If readPipe is called, the current character must be "|"
    if (reader.hasNext && reader.next == '|') {
        reader.advance()
        return LogicalOrToken(charNum, lineNum)
    } else {
        return PipeToken(charNum, lineNum)
    }
}

private fun readStringLiteral(reader: LL1StatefulReader): StringLiteralToken {
    // If readPipe is called, the current character must be "\""
    val charNum = reader.charNum
    val lineNum = reader.lineNum

    reader.advance()

    var str = StringBuilder()
    while (reader.current != '"') {
        str.append(reader.current)
        reader.advance()
    }

    return StringLiteralToken(str.toString(), charNum, lineNum)
}

/**
 * Tokenize the input stream into a list of tokens.
 */
fun createTokens(input: Reader): List<Token> {
    val tokens: MutableList<Token> = mutableListOf()
    val reader = LL1StatefulReader(input)

    while (reader.hasCurrent) {
        val charNum = reader.charNum
        val lineNum = reader.lineNum

        when (reader.current) {
            '+' -> tokens.add(PlusToken(charNum, lineNum))
            '-' -> tokens.add(readMinus(reader))
            '*' -> tokens.add(AsteriskToken(charNum, lineNum))
            '/' -> tokens.add(ForwardSlashToken(charNum, lineNum))
            '^' -> tokens.add(CaretToken(charNum, lineNum))
            '=' -> tokens.add(readEquals(reader))
            '<' -> tokens.add(readLessThan(reader))
            '>' -> tokens.add(readGreaterThan(reader))
            '!' -> tokens.add(readBang(reader))
            '&' -> tokens.add(readAmpersand(reader))
            '|' -> tokens.add(readPipe(reader))
            '(' -> tokens.add(LeftParenToken(charNum, lineNum))
            ')' -> tokens.add(RightParenToken(charNum, lineNum))
            '{' -> tokens.add(LeftBraceToken(charNum, lineNum))
            '}' -> tokens.add(RightBraceToken(charNum, lineNum))
            '[' -> tokens.add(LeftBracketToken(charNum, lineNum))
            ']' -> tokens.add(RightBracketToken(charNum, lineNum))
            '"' -> tokens.add(readStringLiteral(reader))
            ',' -> tokens.add(CommaToken(charNum, lineNum))
            ':' -> tokens.add(ColonToken(charNum, lineNum))
            in '0'..'9' -> tokens.add(readNumber(reader))
            else -> {
                // Identifiers and keywords must begin with an alphabetic character or underscore.
                if (reader.current.isValidIdentifierCharacter()) {
                    tokens.add(readString(reader))
                } else if (!reader.current.isWhitespace()) {
                    throw LexicalException("Unknown character ${reader.current}", charNum, lineNum)
                }
            }
        }

        reader.advance()
    }

    return tokens
}
