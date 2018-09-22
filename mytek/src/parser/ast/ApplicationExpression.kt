package myte.parser.ast

import myte.shared.*

class ApplicationExpression(
    val func: Expression,
    val args: List<Expression>,
    val callLocation: Location
) : Expression(func.startLocation)
