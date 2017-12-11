package myte.parser.ast

data class LogicalAndExpression(val left: Expression, val right: Expression) : Expression()
