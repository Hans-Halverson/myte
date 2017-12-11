package myte.parser.ast

data class ExponentExpression(val base: Expression, val exponent: Expression) : Expression()
