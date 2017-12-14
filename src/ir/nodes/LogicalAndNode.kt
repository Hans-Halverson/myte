package myte.ir.nodes

import myte.shared.*

data class LogicalAndNode(val left: IRNode, val right: IRNode) : IRNode(BoolType)
