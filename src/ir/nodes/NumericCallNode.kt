package myte.ir.nodes

import myte.shared.*

data class NumericCallNode(val func: Identifier, val actualArgs: List<IRNode>) : IRNode(FloatType)
