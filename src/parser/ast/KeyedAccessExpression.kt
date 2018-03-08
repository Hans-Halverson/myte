package myte.parser.ast

import myte.shared.*

data class KeyedAccessExpression(
    val container: Expression,
    val key: Expression,
    val accessLocation: Location
): Expression(container.startLocation)
