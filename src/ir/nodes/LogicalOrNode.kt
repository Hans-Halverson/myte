package myte.ir.nodes

import myte.shared.*

data class LogicalOrNode(val left: IRNode, val right: IRNode) : IRNode(BoolType)
