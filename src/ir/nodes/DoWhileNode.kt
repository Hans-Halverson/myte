package myte.ir.nodes

import myte.shared.*

data class DoWhileNode(val cond: IRNode, val body: IRNode) : IRNode(UnitType)
