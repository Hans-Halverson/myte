package myte.ir.nodes

import myte.eval.values.*
import myte.shared.*

sealed class BinaryMathOperatorNode(val left: IRNode, val right: IRNode) : IRNode(FloatTypeExpression) {
	abstract fun computeInt(left: Int, right: Int): Int
	abstract fun computeFloat(left: Double, right: Double): Double

	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		left.map(func)
		right.map(func)
	}
}

class AddNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right) {
	override fun computeInt(left: Int, right: Int): Int = left + right
	override fun computeFloat(left: Double, right: Double): Double = left + right
}

class SubtractNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right) {
	override fun computeInt(left: Int, right: Int): Int = left - right
	override fun computeFloat(left: Double, right: Double): Double = left - right
}

class MultiplyNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right) {
	override fun computeInt(left: Int, right: Int): Int = left * right	
	override fun computeFloat(left: Double, right: Double): Double = left * right
}

class DivideNode(numer: IRNode, denom: IRNode) : BinaryMathOperatorNode(numer, denom) {
	override fun computeInt(left: Int, right: Int): Int = left / right	
	override fun computeFloat(left: Double, right: Double): Double = left / right
}

class ExponentNode(base: IRNode, exponent: IRNode) : BinaryMathOperatorNode(base, exponent) {
	override fun computeInt(left: Int, right: Int): Int = Math.pow(left.toDouble(), right.toDouble()).toInt()
	override fun computeFloat(left: Double, right: Double): Double = Math.pow(left, right)
}