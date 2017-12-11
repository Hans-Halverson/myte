package myte.ir.nodes

import myte.shared.*

data class VariableDefinitionNode(val ident: Identifier, val expr: IRNode) : IRNode()
