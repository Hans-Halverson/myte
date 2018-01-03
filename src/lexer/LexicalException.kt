package myte.lexer

import myte.shared.*

class LexicalException(
    message: String,
    charNum: Int,
    lineNum: Int
) : ExceptionWithContext(message, Context(charNum, lineNum))
