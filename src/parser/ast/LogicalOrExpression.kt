package myte.parser.ast

class LogicalOrExpression(
    val left: Expression,
    val right: Expression
) : Expression(left.startLocation)
