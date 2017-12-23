package myte.parser

import myte.lexer.*

class Tokenizer(val tokens: List<Token>) {
    private var currentIndex = 0

    /**
     * Returns the current token.
     *
     * @return the current token
     * @throws ParseEOFException if the tokenizer is at the end of the stream of tokens
     */
    val current: Token
        get() {
            if (currentIndex < tokens.size) {
                return tokens.get(currentIndex)
            } else {
                throw ParseEOFException()
            }
        }

    /**
     * Whether the tokenizer has reached the end of the stream of tokens or not.
     */
    val reachedEnd: Boolean
        get() = currentIndex == tokens.size

    /**
     * Advances the tokenizer by one token.
     *
     * @return the current token immediately before the tokenizer was advanced
     * @throws ParseEOFException if the tokenizer is already at the end of the stream of tokens
     */
    fun next(): Token {
        if (currentIndex == tokens.size) {
            throw ParseEOFException()
        } else {
            return tokens.get(currentIndex++)
        }
    }
}
