package myte.ir.nodes

import myte.shared.*

data class ForNode(val init: IRNode?, val cond: IRNode?, val update: IRNode?, val body: IRNode) : IRNode(UnitType)
