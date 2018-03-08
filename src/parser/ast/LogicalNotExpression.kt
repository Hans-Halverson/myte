package myte.parser.ast

import myte.shared.*

class LogicalNotExpression(
    val expr: Expression,
    startLocation: Location
) : Expression(startLocation)
