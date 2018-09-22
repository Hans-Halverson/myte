package myte.lexer

import java.io.Reader
import java.math.BigDecimal
import java.math.BigInteger

import myte.shared.*

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
                throw LexicalException("Unexpected EOF", currentLocation)
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
                throw LexicalException("Unexpected EOF", currentLocation)
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

    val currentLocation: Location
        get() = Location(charNum, lineNum, fileName)

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
private fun getKeywordToken(str: String, location: Location): Token? {
    return when (str) {
        "true" -> TrueToken(location)
        "false" -> FalseToken(location)
        "type" -> TypeToken(location)
        "let" -> LetToken(location)
        "const" -> ConstToken(location)
        "def" -> DefToken(location)
        "fun" -> FunToken(location)
        "if" -> IfToken(location)
        "else" -> ElseToken(location)
        "while" -> WhileToken(location)
        "do" -> DoToken(location)
        "for" -> ForToken(location)
        "forEach" -> ForEachToken(location)
        "in" -> InToken(location)
        "match" -> MatchToken(location)
        "when" -> WhenToken(location)
        "return" -> ReturnToken(location)
        "break" -> BreakToken(location)
        "continue" -> ContinueToken(location)
        "unit" -> UnitToken(location)
        "bool" -> BoolToken(location)
        "string" -> StringTypeToken(location)
        "byte" -> ByteToken(location)
        "int" -> IntToken(location)
        "float" -> FloatToken(location)
        "double" -> DoubleToken(location)
        "vec" -> VecToken(location)
        "map" -> MapToken(location)
        "set" -> SetToken(location)
        "package" -> PackageToken(location)
        "import" -> ImportToken(location)
        "as" -> AsToken(location)
        "implement" -> ImplementToken(location)
        "extends" -> ExtendsToken(location)
        "trait" -> TraitToken(location)
        "sig" -> SigToken(location)
        "static" -> StaticToken(location)
        "mut" -> MutToken(location)
        "__builtin" -> BuiltinToken(location)
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
private fun readNumber(reader: LL1StatefulReader): List<Token> {
    var numberString = StringBuilder()

    val location = reader.currentLocation

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
        return listOf(IntegralLiteralToken(BigInteger(numberString.toString()), location))
    }

    // If next token is the beginning of an identifier, this is actually an access after an int
    if (reader.hasNext && (reader.next.isLetter() || reader.next.equals('_'))) {
        val numberWithDecimalPoint =  numberString.toString()
        val intString = numberWithDecimalPoint.substring(0, numberWithDecimalPoint.length - 1)
        return listOf(IntegralLiteralToken(BigInteger(intString), location),
                PeriodToken(reader.currentLocation))
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
        return listOf(DecimalLiteralToken(BigDecimal(numberString.toString()), location))
    }

    // Read exponent sign
    if (reader.hasNext && (reader.next == '+' || reader.next == '-')) {
        numberString.append(reader.next)
        reader.advance()
    } else {
        return listOf(DecimalLiteralToken(BigDecimal(numberString.toString()), location))
    }

    if (reader.hasNext && reader.next !in '0'..'9') {
        throw LexicalException("Malformed float ${numberString.toString()}", location)
    }

    // Read exponent digits
    while (reader.hasNext && reader.next in '0'..'9') {
        numberString.append(reader.next)
        reader.advance()
    }

    return listOf(DecimalLiteralToken(BigDecimal(numberString.toString()), location))
}

/** 
 * Read a single string into either an identifier token, or a keyword token if the string is
 * a keyword in the keywords map.
 */
private fun readString(reader: LL1StatefulReader): Token {
    var str = StringBuilder()

    val location = reader.currentLocation

    // If readString is called, the current character must be valid
    str.append(reader.current)

    while (reader.hasNext && reader.next.isValidIdentifierCharacter()) {
        str.append(reader.next)
        reader.advance()
    }

    // Check if string is a keyword
    var string = str.toString()
    var keywordToken = getKeywordToken(string, location)

    return keywordToken ?: IdentifierToken(string, location)
}

// Identifiers in myte consist of only alphanumeric characters and underscores.
private fun Char.isValidIdentifierCharacter(): Boolean = this.isLetterOrDigit() || this.equals('_')

private fun readMinus(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readMinus is called, the current character must be "-"
    if (reader.hasNext && reader.next == '>') {
        reader.advance()
        return ArrowToken(location)
    } else {
        return MinusToken(location)
    }
}

private fun readEquals(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readEquals is called, the current character must be '='
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return DoubleEqualsToken(location)
    } else {
        return EqualsToken(location)
    }
}

private fun readLessThan(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readLessThan is called, the current character must be '<'
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return LessThanOrEqualToken(location)
    } else {
        return LessThanToken(location)
    }
}

private fun readGreaterThan(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readGreaterThan is called, the current character must be '>'
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return GreaterThanOrEqualToken(location)
    } else {
        return GreaterThanToken(location)
    }
}

private fun readBang(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readBang is called, the current character must be "!"
    if (reader.hasNext && reader.next == '=') {
        reader.advance()
        return NotEqualsToken(location)
    } else {
        return LogicalNotToken(location)
    }
}

private fun readAmpersand(reader: LL1StatefulReader): LogicalAndToken {
    val location = reader.currentLocation

    // If readAmpersand is called, the current character must be "&"
    if (reader.hasNext && reader.next == '&') {
        reader.advance()
        return LogicalAndToken(location)
    } else {
        throw LexicalException("Unknown symbol &${reader.next}", location)
    }
}

private fun readPipe(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readPipe is called, the current character must be "|"
    if (reader.hasNext && reader.next == '|') {
        reader.advance()
        return LogicalOrToken(location)
    } else if (reader.hasNext && reader.next == ']') {
        reader.advance()
        return RightMapLiteralToken(location)
    } else if (reader.hasNext && reader.next == '}') {
        reader.advance()
        return RightSetLiteralToken(location)
    } else {
        return PipeToken(location)
    }
}

private fun readLeftBrace(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readLeftBrace is called, the current character must be "{"
    if (reader.hasNext && reader.next == '|') {
        reader.advance()
        return LeftSetLiteralToken(location)
    } else {
        return LeftBraceToken(location)
    }
}

private fun readLeftBracket(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readLeftBracket is called, the current character must be "["
    if (reader.hasNext && reader.next == '|') {
        reader.advance()
        return LeftMapLiteralToken(location)
    } else {
        return LeftBracketToken(location)
    }
}

private fun readColon(reader: LL1StatefulReader): Token {
    val location = reader.currentLocation

    // If readColon is called, the current character must be ":"
    if (reader.hasNext && reader.next == ':') {
        reader.advance()
        return ScopeToken(location)
    } else {
        return ColonToken(location)
    }
}

private fun readSlash(reader: LL1StatefulReader): Token? {
    val location = reader.currentLocation

    // If readColon is called, the current character must be "/"
    if (reader.hasNext && reader.next == '/') {
        reader.advance()
        readLineComment(reader)
        return null
    } else if (reader.hasNext && reader.next == '*') {
        reader.advance()
        readBlockComment(reader)
        return null
    } else {
        return ForwardSlashToken(location)
    }
}

private fun readLineComment(reader: LL1StatefulReader) {
    // Read until next new line character, consuming new line
    while (reader.current != '\n') {
        reader.advance()
    }
}

private fun readBlockComment(reader: LL1StatefulReader) {
    // Keep consuming tokens until a "*/" is seen
    var seenAsterisk = false
    while (true) {
        if (reader.current == '*') {
            seenAsterisk = true
        } else if (seenAsterisk && reader.current == '/') {
            reader.advance()
            break
        } else {
            seenAsterisk = false
        }

        if (reader.hasNext) {
            reader.advance()
        } else {
            throw UnexpectedEOFException()
        }
    }
}

private fun readStringLiteral(reader: LL1StatefulReader): StringLiteralToken {
    // If readPipe is called, the current character must be "\""
    val location = reader.currentLocation

    reader.advance()

    var str = StringBuilder()
    while (reader.current != '"') {
        // Check for escaped characters
        if (reader.current == '\\' && reader.hasNext) {
            if (reader.next == '\\') {
                reader.advance()
                str.append("\\")
            } else if (reader.next == 'n') {
                reader.advance()
                str.append("\n")
            } else if (reader.next == 't') {
                reader.advance()
                str.append("\t")
            } else if (reader.next == 'b') {
                reader.advance()
                str.append("\b")
            } else if (reader.next == 'r') {
                reader.advance()
                str.append("\r")
            } else if (reader.next == '"') {
                reader.advance()
                str.append("\"")
            } else {
                str.append(reader.current)
            }
        } else {
            str.append(reader.current)
        }

        if (reader.hasNext) {
            reader.advance()
        } else {
            throw UnexpectedEOFException()
        }
    }

    return StringLiteralToken(str.toString(), location)
}

/**
 * Tokenize the input stream into a list of tokens.
 */
fun createTokens(input: Reader, fileName: String?): List<Token> {
    val tokens: MutableList<Token> = mutableListOf()
    val reader = LL1StatefulReader(input, fileName)

    while (reader.hasCurrent) {
        val location = reader.currentLocation

        when (reader.current) {
            '+' -> tokens.add(PlusToken(location))
            '-' -> tokens.add(readMinus(reader))
            '*' -> tokens.add(AsteriskToken(location))
            '^' -> tokens.add(CaretToken(location))
            '%' -> tokens.add(PercentToken(location))
            '=' -> tokens.add(readEquals(reader))
            '<' -> tokens.add(readLessThan(reader))
            '>' -> tokens.add(readGreaterThan(reader))
            '!' -> tokens.add(readBang(reader))
            '&' -> tokens.add(readAmpersand(reader))
            '|' -> tokens.add(readPipe(reader))
            '(' -> tokens.add(LeftParenToken(location))
            ')' -> tokens.add(RightParenToken(location))
            '{' -> tokens.add(readLeftBrace(reader))
            '}' -> tokens.add(RightBraceToken(location))
            '[' -> tokens.add(readLeftBracket(reader))
            ']' -> tokens.add(RightBracketToken(location))
            '"' -> tokens.add(readStringLiteral(reader))
            '.' -> tokens.add(PeriodToken(location))
            ',' -> tokens.add(CommaToken(location))
            ':' -> tokens.add(readColon(reader))
            '/' -> {
                // Only add token if one exists. This could read a comment and generate no token.
                val token = readSlash(reader)
                if (token != null) {
                    tokens.add(token)
                }
            }
            in '0'..'9' -> tokens.addAll(readNumber(reader))
            else -> {
                // Identifiers and keywords must begin with an alphabetic character or underscore.
                if (reader.current.isValidIdentifierCharacter()) {
                    tokens.add(readString(reader))
                } else if (!reader.current.isWhitespace()) {
                    throw LexicalException("Unknown character ${reader.current}", location)
                }
            }
        }

        reader.advance()
    }

    return tokens
}
