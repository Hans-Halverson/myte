package myte.parser.ast

sealed class NumberLiteral : Expression()

data class IntLiteral(val num: Int): NumberLiteral()

data class FloatLiteral(val num: Double) : NumberLiteral()
