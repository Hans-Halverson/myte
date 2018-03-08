package myte.parser.ast

import myte.shared.*

data class ReturnStatement(
    val expr: Expression?,
    val returnLocation: Location
) : Statement(returnLocation)
