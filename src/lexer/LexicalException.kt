package myte.lexer

import myte.shared.*

class LexicalException(
    message: String,
    location: Location
) : ExceptionWithLocation(message, location)
