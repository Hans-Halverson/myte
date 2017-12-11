package myte.parser.ast

data class AddExpression(val left: Expression, val right: Expression) : Expression()
