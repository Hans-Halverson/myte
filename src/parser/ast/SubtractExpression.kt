package myte.parser.ast

data class SubtractExpression(val left: Expression, val right: Expression) : Expression()
