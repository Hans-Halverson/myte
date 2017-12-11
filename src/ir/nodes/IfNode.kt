package myte.ir.nodes

data class IfNode(val cond: IRBooleanNode, val conseq: IRNode, val altern: IRNode?) : IRNode()
