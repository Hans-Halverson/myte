package myte.ir.nodes

import myte.shared.*

sealed class ComparisonNode(val left: IRNode, val right: IRNode) : IRNode(BoolType) {
	abstract fun compare(a: Double, b: Double): Boolean
}

class LessThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a < b)
}

class LessThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a <= b)
}

class GreaterThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a > b)
}

class GreaterThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a >= b)
}
