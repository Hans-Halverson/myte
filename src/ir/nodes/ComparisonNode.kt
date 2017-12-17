package myte.ir.nodes

import myte.shared.*

sealed class ComparisonNode(val left: IRNode, val right: IRNode, val numType: NumberType) : IRNode(BoolType) {
	abstract fun compareInts(a: Int, b: Int): Boolean
	abstract fun compareFloats(a: Double, b: Double): Boolean
}

class LessThanNode(left: IRNode, right: IRNode, numType: NumberType) : ComparisonNode(left, right, numType) {
	override fun compareInts(a: Int, b: Int) = (a < b)
	override fun compareFloats(a: Double, b: Double) = (a < b)
}

class LessThanOrEqualNode(left: IRNode, right: IRNode, numType: NumberType) : ComparisonNode(left, right, numType) {
	override fun compareInts(a: Int, b: Int) = (a <= b)
	override fun compareFloats(a: Double, b: Double) = (a <= b)
}

class GreaterThanNode(left: IRNode, right: IRNode, numType: NumberType) : ComparisonNode(left, right, numType) {
	override fun compareInts(a: Int, b: Int) = (a > b)
	override fun compareFloats(a: Double, b: Double) = (a > b)
}

class GreaterThanOrEqualNode(left: IRNode, right: IRNode, numType: NumberType) : ComparisonNode(left, right, numType) {
	override fun compareInts(a: Int, b: Int) = (a >= b)
	override fun compareFloats(a: Double, b: Double) = (a >= b)
}
