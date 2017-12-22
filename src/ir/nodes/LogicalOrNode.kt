package myte.ir.nodes

import myte.shared.*

data class LogicalOrNode(val left: IRNode, val right: IRNode) : IRNode(BoolTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		left.map(func)
		right.map(func)
	}
}
