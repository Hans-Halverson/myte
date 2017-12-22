package myte.ir.nodes

import myte.shared.*

sealed class UnaryMathOperatorNode(val node: IRNode) : IRNode(newTypeVariable()) {
	abstract fun computeInt(num: Int): Int
	abstract fun computeFloat(num: Double): Double

	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		node.map(func)
	}
}

class IdentityNode(node: IRNode) : UnaryMathOperatorNode(node) {
	override fun computeInt(num: Int): Int = num
	override fun computeFloat(num: Double): Double = num
}

class NegateNode(node: IRNode) : UnaryMathOperatorNode(node) {
	override fun computeInt(num: Int): Int = -num
	override fun computeFloat(num: Double): Double = -num
}
