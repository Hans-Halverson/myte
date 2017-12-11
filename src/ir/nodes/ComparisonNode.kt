package myte.ir.nodes


abstract class ComparisonNode(val left: IRNumericNode, val right: IRNumericNode) : IRBooleanNode() {
	abstract fun compare(a: Double, b: Double): Boolean
}

class LessThanNode(left: IRNumericNode, right: IRNumericNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a < b)
}

class LessThanOrEqualNode(left: IRNumericNode, right: IRNumericNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a <= b)
}

class GreaterThanNode(left: IRNumericNode, right: IRNumericNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a > b)
}

class GreaterThanOrEqualNode(left: IRNumericNode, right: IRNumericNode) : ComparisonNode(left, right) {
	override fun compare(a: Double, b: Double) = (a >= b)
}
