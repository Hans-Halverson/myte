package myte.ir.nodes

import myte.shared.*

data class WhileNode(val cond: IRNode, val body: IRNode) : IRNode(UnitType)
