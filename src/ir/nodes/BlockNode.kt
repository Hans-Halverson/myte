package myte.ir.nodes

import myte.shared.*

data class BlockNode(val nodes: List<IRNode>) : IRNode(UnitTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		nodes.map { node -> node.map(func) }
	}
}
