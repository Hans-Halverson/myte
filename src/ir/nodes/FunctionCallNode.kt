package myte.ir.nodes

import myte.shared.*

data class FunctionCallNode(val func: Identifier, val actualArgs: List<IRNode>) : IRNode()
