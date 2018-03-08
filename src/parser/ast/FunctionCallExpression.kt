package myte.parser.ast

import myte.shared.*

data class FunctionCallExpression(
    val func: Expression,
    val actualArgs: List<Expression>,
    val identLocation: Location
) : Expression(identLocation)
