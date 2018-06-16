package myte.parser.ast

sealed class BinaryMathOperatorExpression(
    val left: Expression,
    val right: Expression
) : Expression(left.startLocation)

class AddExpression(
    left: Expression,
    right: Expression
) : BinaryMathOperatorExpression(left, right)

class SubtractExpression(
    left: Expression,
    right: Expression
) : BinaryMathOperatorExpression(left, right)

class MultiplyExpression(
    left: Expression,
    right: Expression
) : BinaryMathOperatorExpression(left, right)

class DivideExpression(
    numer: Expression,
    denom: Expression
) : BinaryMathOperatorExpression(numer, denom)

class ExponentExpression(
    base: Expression,
    exponent: Expression
) : BinaryMathOperatorExpression(base, exponent)

class RemainderExpression(
    left: Expression,
    right: Expression
) : BinaryMathOperatorExpression(left, right)
