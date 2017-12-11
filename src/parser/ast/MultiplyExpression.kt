package myte.parser.ast

data class MultiplyExpression(val left: Expression, val right: Expression) : Expression()
