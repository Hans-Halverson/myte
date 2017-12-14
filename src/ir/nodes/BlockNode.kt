package myte.ir.nodes

import myte.shared.*

data class BlockNode(val nodes: List<IRNode>) : IRNode(UnitType)
