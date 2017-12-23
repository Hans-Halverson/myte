package myte.parser

import myte.lexer.*

class ParseException(message: String) : Exception(message) {
    constructor(token: Token?) : this("Unexpected ${token ?: "EOF"} encountered")
    constructor(expected: TokenType, actual: TokenType) : this("\"$expected\" expected, "
            +" but \"$actual\" found")
}

class ParseEOFException() : Exception("Unexpected EOF encountered")
