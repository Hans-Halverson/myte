package myte.parser.ast

import myte.shared.*

data class FunctionDefinitionExpression(val ident: Identifier, val formalArgs: List<Identifier>, val body: Expression) : Statement()
