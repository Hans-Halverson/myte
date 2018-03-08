package myte.parser.ast

sealed class EqualityExpression(
    val left: Expression,
    val right: Expression
) : Expression(left.startLocation)

class EqualsExpression(left: Expression, right: Expression) : EqualityExpression(left, right)

class NotEqualsExpression(left: Expression, right: Expression) : EqualityExpression(left, right)
