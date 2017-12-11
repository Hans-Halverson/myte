package myte.parser.ast

data class LogicalOrExpression(val left: Expression, val right: Expression) : Expression()
