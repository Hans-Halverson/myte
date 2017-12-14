package myte.ir.nodes

import myte.eval.values.*
import myte.shared.*

sealed class BinaryMathOperatorNode(val left: IRNode, val right: IRNode) : IRNode(FloatType) {
	abstract fun compute(left: Double, right: Double): Double
}

class AddNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right) {
	override fun compute(left: Double, right: Double): Double = left + right
}

class SubtractNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right) {
	override fun compute(left: Double, right: Double): Double = left - right
}

class MultiplyNode(left: IRNode, right: IRNode) : BinaryMathOperatorNode(left, right) {
	override fun compute(left: Double, right: Double): Double = left * right
}

class DivideNode(numer: IRNode, denom: IRNode) : BinaryMathOperatorNode(numer, denom) {
	override fun compute(left: Double, right: Double): Double = left / right
}

class ExponentNode(base: IRNode, exponent: IRNode) : BinaryMathOperatorNode(base, exponent) {
	override fun compute(left: Double, right: Double): Double = Math.pow(left, right)
}