package myte.ir.nodes

import myte.shared.*

data class BooleanLiteralNode(val bool: Boolean) : IRNode(BoolType)
