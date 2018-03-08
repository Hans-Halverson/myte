package myte.parser.ast

import myte.shared.*

data class AccessExpression(
    val left: Expression,
    val right: Expression,
    val dotLocation: Location
): Expression(left.startLocation)
