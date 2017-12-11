package myte.ir.nodes

import myte.shared.*

data class AssignmentNode(val ident: Identifier, val expr: IRNode) : IRNode()
