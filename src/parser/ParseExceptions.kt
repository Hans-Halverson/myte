package myte.parser

import myte.lexer.*
import myte.shared.*

class ParseException(
    message: String,
    context: Context
) : ExceptionWithContext(message, context) {
    constructor(token: Token)
            : this("Unexpected ${token} encountered", token.context)
    constructor(message: String, token: Token)
            : this(message, token.context)
    constructor(expected: TokenType, actual: TokenType, token: Token)
            : this("\"$expected\" expected, but \"$actual\" found", token.context)
}

class ParseEOFException() : Exception("Unexpected EOF encountered")
