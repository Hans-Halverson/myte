package myte.parser.ast

data class NotEqualsExpression(val left: Expression, val right: Expression) : Expression()
