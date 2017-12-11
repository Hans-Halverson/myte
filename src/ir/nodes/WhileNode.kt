package myte.ir.nodes

data class WhileNode(val cond: IRBooleanNode, val stmt: IRNode) : IRNode()
