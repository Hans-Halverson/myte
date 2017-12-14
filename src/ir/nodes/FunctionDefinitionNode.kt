package myte.ir.nodes

import myte.shared.*

data class FunctionDefinitionNode(val ident: Identifier, val formalArgs: List<Identifier>, val stmt: IRNode) : IRNode(UnitType)
