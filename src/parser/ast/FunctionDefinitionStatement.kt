package myte.parser.ast

import myte.shared.*

data class FunctionDefinitionStatement(val ident: Identifier, val formalArgs: List<Identifier>, val body: Statement) : Statement()
