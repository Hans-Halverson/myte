package myte.parser.ast

data class TupleLiteralExpression(val elements: List<Expression>): Expression()
