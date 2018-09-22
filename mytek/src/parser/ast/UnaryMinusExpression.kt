package myte.parser.ast

import myte.shared.*

class UnaryMinusExpression(
    val expr: Expression,
    startLocation: Location
) : Expression(startLocation)
