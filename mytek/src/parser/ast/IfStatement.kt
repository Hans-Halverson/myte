package myte.parser.ast

import myte.shared.*

class IfStatement(
    val cond: Expression,
    val conseq: Statement,
    val altern: Statement?,
    startLocation: Location
) : Expression(startLocation)
