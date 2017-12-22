package myte.ir.nodes

import myte.shared.*

data class LogicalNotNode(val node: IRNode) : IRNode(BoolTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		node.map(func)
	}
}
