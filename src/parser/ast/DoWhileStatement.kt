package myte.parser.ast

import myte.shared.*

class DoWhileStatement(
    val cond: Expression,
    val body: Statement,
    startLocation: Location
) : Statement(startLocation)
