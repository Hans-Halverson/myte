package myte.ir.nodes

data class DoWhileNode(val cond: IRBooleanNode, val stmt: IRNode) : IRNode()
