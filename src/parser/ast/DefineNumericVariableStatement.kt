package myte.parser.ast

import myte.shared.*

data class DefineNumericVariableStatement(val ident: Identifier, val expr: Expression) : Statement()
