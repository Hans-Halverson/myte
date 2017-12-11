package myte.parser.ast

data class EqualsExpression(val left: Expression, val right: Expression) : Expression()
