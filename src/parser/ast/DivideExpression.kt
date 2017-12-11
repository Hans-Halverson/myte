package myte.parser.ast

data class DivideExpression(val numer: Expression, val denom: Expression) : Expression()
