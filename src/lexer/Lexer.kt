package myte.lexer

import myte.shared.*

import java.io.Reader

/**
 * A reader which allows the user to inspect the current and next character from an input stream.
 * 
 * @property reader a reader that supplies the input stream
 * @property fileName the (optional) name of the file being read, or none if not reading from a file
 */
private class LL1StatefulReader(private val reader: Reader, private val fileName: String?) {
    private var currentChar: Int
    private var nextChar: Int

    private var charNum: Int = 1
    private var lineNum: Int = 1

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
                throw LexicalException("Unexpected EOF", currentContext)
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
                throw LexicalException("Unexpected EOF", currentContext)
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

    val currentContext: Context
        get() = Context(charNum, lineNum, fileName)

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
private fun getKeywordToken(str: String, context: Context): Token? {
    return when (str) {
        "true" -> TrueToken(context)
        "false" -> FalseToken(context)
        "type" -> TypeToken(context)
        "let" -> LetToken(context)
        "const" -> ConstToken(context)
        "def" -> DefToken(context)
        "num" -> NumToken(context)
        "if" -> IfToken(context)
        "else" -> ElseToken(context)
        "while" -> WhileToken(context)
        "do" -> DoToken(context)
        "for" -> ForToken(context)
        "match" -> MatchToken(context)
        "return" -> ReturnToken(context)
        "break" -> BreakToken(context)
        "continue" -> ContinueToken(context)
        "unit" -> UnitToken(context)
        "bool" -> BoolToken(context)
        "string" -> StringTypeToken(context)
        "int" -> IntToken(context)
        "float" -> FloatToken(context)
        "vec" -> VecToken(context)
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

    val context = reader.currentContext

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
        return stringToInt(numberString.toString(), context)
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
        return stringToFloat(numberString.toString(), context)
    }

    // Read exponent sign
    if (reader.hasNext && (reader.next == '+' || reader.next == '-')) {
        numberString.append(reader.next)
        reader.advance()
    } else {
        return stringToFloat(numberString.toString(), context)
    }

    if (reader.hasNext && reader.next !in '0'..'9') {
        throw LexicalException("Malformed float ${numberString.toString()}", context)
    }

    // Read exponent digits
    while (reader.hasNext && reader.next in '0'..'9') {
        numberString.append(reader.next)
        reader.advance()
    }

    return stringToFloat(numberString.toString(), context)
}

/**
 * Convert a string to an int token.
 * @throws LexicalException if the string does not represent a valid int
 */
private fun stringToInt(str: String, context: Context): IntLiteralToken {
    try {
        return IntLiteralToken(str.toInt(), context)
    } catch (e: NumberFormatException) {
        throw LexicalException("Malformed int ${str}", context)
    }
}

/**
 * Convert a string to a float token.
 * @throws LexicalException if the string does not represent a valid float
 */
private fun stringToFloat(str: String, context: Context): FloatLiteralToken {
    try {
        return FloatLiteralToken(str.toDouble(), context)
    } catch (e: NumberFormatException) {
        throw LexicalException("Malformed float ${str}", context)
    }
}

/** 
 * Read a single string into either an identifier token, or a keyword token if the string is
 * a keyword in the keywords map.
 */
private fun readString(reader: LL1StatefulReader): Token {
    var str = StringBuilder()

    val context = reader.currentContext

    // If readString is called, the current character must be valid
    str.append(reader.current)

    while (reader.hasNext && reader.next.isValidIdentifierCharacter()) {
        str.append(reader.next)
        reader.advance()
    }

    // Check if string is a keyword
    var string = str.toString()
    var keywordToken = getKeywordToken(string, context)

    return keywordToken ?: IdentifierToken(string, context)
}

// Identifiers in myte consist of only alphanumeric characters and underscores.
private fun Char.isValidIdentifierCharacter(): Boolean = this.isLetterOrDigit() || this.equals('_')

private fun readMinus(reader: LL1StatefulReader): Token {
    val context = reader.currentContext

    // If readMinus is called, the current character must be "-"
    if (reader.hasNext && reader.next == '>') {
        reader.advance()
        return ArrowToken(context)
    } else {
        return MinusToken(context)
    }
}

private fun readEquals(reader: LL1StatefulReader): Token {
    val context = reader.currentContext

    // If readEquals is called, the current character must be '='
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return DoubleEqualsToken(context)
    } else {
        return EqualsToken(context)
    }
}

private fun readLessThan(reader: LL1StatefulReader): Token {
    val context = reader.currentContext

    // If readLessThan is called, the current character must be '<'
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return LessThanOrEqualToken(context)
    } else {
        return LessThanToken(context)
    }
}

private fun readGreaterThan(reader: LL1StatefulReader): Token {
    val context = reader.currentContext

    // If readGreaterThan is called, the current character must be '>'
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return GreaterThanOrEqualToken(context)
    } else {
        return GreaterThanToken(context)
    }
}

private fun readBang(reader: LL1StatefulReader): Token {
    val context = reader.currentContext

    // If readBang is called, the current character must be "!"
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return NotEqualsToken(context)
    } else {
        return LogicalNotToken(context)
    }
}

private fun readAmpersand(reader: LL1StatefulReader): LogicalAndToken {
    val context = reader.currentContext

    // If readAmpersand is called, the current character must be "&"
    if (reader.hasNext && reader.next == '&') {
        reader.advance()
        return LogicalAndToken(context)
    } else {
        throw LexicalException("Unexpected token encountered &${reader.next}", context)
    }
}

private fun readPipe(reader: LL1StatefulReader): Token {
    val context = reader.currentContext

    // If readPipe is called, the current character must be "|"
    if (reader.hasNext && reader.next == '|') {
        reader.advance()
        return LogicalOrToken(context)
    } else {
        return PipeToken(context)
    }
}

private fun readStringLiteral(reader: LL1StatefulReader): StringLiteralToken {
    // If readPipe is called, the current character must be "\""
    val context = reader.currentContext

    reader.advance()

    var str = StringBuilder()
    while (reader.current != '"') {
        str.append(reader.current)
        reader.advance()
    }

    return StringLiteralToken(str.toString(), context)
}

/**
 * Tokenize the input stream into a list of tokens.
 */
fun createTokens(input: Reader, fileName: String?): List<Token> {
    val tokens: MutableList<Token> = mutableListOf()
    val reader = LL1StatefulReader(input, fileName)

    while (reader.hasCurrent) {
        val context = reader.currentContext

        when (reader.current) {
            '+' -> tokens.add(PlusToken(context))
            '-' -> tokens.add(readMinus(reader))
            '*' -> tokens.add(AsteriskToken(context))
            '/' -> tokens.add(ForwardSlashToken(context))
            '^' -> tokens.add(CaretToken(context))
            '=' -> tokens.add(readEquals(reader))
            '<' -> tokens.add(readLessThan(reader))
            '>' -> tokens.add(readGreaterThan(reader))
            '!' -> tokens.add(readBang(reader))
            '&' -> tokens.add(readAmpersand(reader))
            '|' -> tokens.add(readPipe(reader))
            '(' -> tokens.add(LeftParenToken(context))
            ')' -> tokens.add(RightParenToken(context))
            '{' -> tokens.add(LeftBraceToken(context))
            '}' -> tokens.add(RightBraceToken(context))
            '[' -> tokens.add(LeftBracketToken(context))
            ']' -> tokens.add(RightBracketToken(context))
            '"' -> tokens.add(readStringLiteral(reader))
            ',' -> tokens.add(CommaToken(context))
            ':' -> tokens.add(ColonToken(context))
            in '0'..'9' -> tokens.add(readNumber(reader))
            else -> {
                // Identifiers and keywords must begin with an alphabetic character or underscore.
                if (reader.current.isValidIdentifierCharacter()) {
                    tokens.add(readString(reader))
                } else if (!reader.current.isWhitespace()) {
                    throw LexicalException("Unknown character ${reader.current}", context)
                }
            }
        }

        reader.advance()
    }

    return tokens
}
