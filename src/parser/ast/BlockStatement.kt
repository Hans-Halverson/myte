package myte.parser.ast

import myte.shared.*

class BlockStatement(
    val stmts: List<Statement>,
    startLocation: Location
) : Expression(startLocation)
