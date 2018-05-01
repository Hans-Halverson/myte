package myte.parser

import myte.lexer.*
import myte.shared.*

class ParseException(
    message: String,
    location: Location
) : ExceptionWithLocation(message, location) {
    constructor(token: Token)
            : this("Unexpected ${token} encountered", token.location)
    constructor(message: String, token: Token)
            : this(message, token.location)
    constructor(expected: TokenType, actual: TokenType, token: Token)
            : this("\"$expected\" expected, but \"$actual\" found", token.location)
}

class AmbiguousEndException() : Exception("EOF encountered with full parse, but current " +
        "expression could continue")