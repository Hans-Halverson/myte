package myte.parser.ast

import myte.shared.*

data class DefineNumericFunctionStatement(val ident: Identifier, val formalArgs: List<Identifier>, val expr: Expression) : Statement()
