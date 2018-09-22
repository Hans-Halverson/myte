package myte.parser.ast

import myte.shared.*

data class AssignmentExpression(
    val lValue: Expression,
    val rValue: Expression,
    val equalsLocation: Location
) : Expression(lValue.startLocation)
