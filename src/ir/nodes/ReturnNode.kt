package myte.ir.nodes

import myte.shared.*

data class ReturnNode(val expr: IRNode?) : IRNode(UnitType)
