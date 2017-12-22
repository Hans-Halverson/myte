package myte.ir.nodes

import myte.shared.*

sealed class EqualityNode(val left: IRNode, val right: IRNode) : IRNode(BoolTypeExpression) {
	abstract fun compare(a: Any, b: Any): Boolean

	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		left.map(func)
		right.map(func)
	}
}

class EqualsNode(left: IRNode, right: IRNode) : EqualityNode(left, right) {
	override fun compare(a: Any, b: Any) = (a == b)
}

class NotEqualsNode(left: IRNode, right: IRNode) : EqualityNode(left, right) {
	override fun compare(a: Any, b: Any) = (a != b)
}
