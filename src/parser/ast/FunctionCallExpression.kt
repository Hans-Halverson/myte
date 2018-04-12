package myte.parser.ast

import myte.shared.*

class FunctionCallExpression(
    val func: Expression,
    val actualArgs: List<Expression>,
    val callLocation: Location,
    startLocation: Location
) : Expression(startLocation)
