package myte.parser.ast

import myte.shared.*

class WhileStatement(
    val cond: Expression,
    val body: Statement,
    startLocation: Location
) : Statement(startLocation)
