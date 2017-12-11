package myte.ir.nodes

data class LogicalOrNode(val left: IRBooleanNode, val right: IRBooleanNode) : IRBooleanNode()
