package myte.ir.nodes

data class ForNode(val init: IRNode?, val cond: IRBooleanNode?, val update: IRNode?, val stmt: IRNode) : IRNode()
