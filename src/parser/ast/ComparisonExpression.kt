package myte.parser.ast

sealed class ComparisonExpression(
    val left: Expression,
    val right: Expression
) : Expression(left.startContext)

class LessThanExpression(
    left: Expression,
    right: Expression
) : ComparisonExpression(left, right)

class LessThanOrEqualExpression(
    left: Expression,
    right: Expression
) : ComparisonExpression(left, right)

class GreaterThanExpression(
    left: Expression,
    right: Expression
) : ComparisonExpression(left, right)

class GreaterThanOrEqualExpression(
    left: Expression,
    right: Expression
) : ComparisonExpression(left, right)
