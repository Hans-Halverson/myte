package myte.ir.nodes

import myte.shared.*

data class LogicalNotNode(val node: IRNode) : IRNode(BoolType)
