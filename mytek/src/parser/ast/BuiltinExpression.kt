package myte.parser.ast

import myte.shared.*

class BuiltinExpression(
    val builtin: String,
    val args: List<Expression>,
    startLocation: Location
) : Expression(startLocation)
