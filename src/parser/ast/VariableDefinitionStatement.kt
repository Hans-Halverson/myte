package myte.parser.ast

import myte.shared.*

data class VariableDefinitionStatement(val ident: Identifier, val expr: Expression) : Statement()
