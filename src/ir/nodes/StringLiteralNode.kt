package myte.ir.nodes

import myte.shared.*

data class StringLiteralNode(val str: String) : IRNode(StringTypeExpression)
