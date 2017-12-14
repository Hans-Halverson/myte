package myte.ir.nodes

import myte.shared.*

sealed class EqualityNode(val left: IRNode, val right: IRNode) : IRNode(BoolType) {
	abstract fun compare(a: Any, b: Any): Boolean
}

class EqualsNode(left: IRNode, right: IRNode) : EqualityNode(left, right) {
	override fun compare(a: Any, b: Any) = (a == b)
}

class NotEqualsNode(left: IRNode, right: IRNode) : EqualityNode(left, right) {
	override fun compare(a: Any, b: Any) = (a != b)
}
