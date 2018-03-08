package myte.parser.ast

class LogicalAndExpression(
    val left: Expression,
    val right: Expression
) : Expression(left.startLocation)
