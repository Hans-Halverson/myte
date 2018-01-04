package myte.parser.ast

import myte.shared.*

class VariableDefinitionStatement(
    val ident: Identifier,
    val expr: Expression,
    val identContext: Context,
    startContext: Context
) : Statement(startContext)
