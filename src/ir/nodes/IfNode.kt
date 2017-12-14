package myte.ir.nodes

import myte.shared.*

data class IfNode(val cond: IRNode, val conseq: IRNode, val altern: IRNode?) : IRNode(UnitType)
