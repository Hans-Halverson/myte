package myte.parser.ast

import myte.shared.*

data class AccessExpression(
    val expr: Expression,
    val field: String,
    val accessLocation: Location
): Expression(expr.startLocation)
