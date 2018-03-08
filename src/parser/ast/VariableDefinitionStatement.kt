package myte.parser.ast

import myte.shared.*

class VariableDefinitionStatement(
    val ident: Identifier,
    val expr: Expression,
    val identLocation: Location,
    startLocation: Location
) : Statement(startLocation)
