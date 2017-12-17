package myte.ir.nodes

import myte.eval.values.*
import myte.shared.*

sealed class BinaryMathOperatorNode(val left: IRNode, val right: IRNode, type: NumberType) : IRNode(type) {
	abstract fun computeInt(left: Int, right: Int): Int
	abstract fun computeFloat(left: Double, right: Double): Double
}

class AddNode(left: IRNode, right: IRNode, type: NumberType) : BinaryMathOperatorNode(left, right, type) {
	override fun computeInt(left: Int, right: Int): Int = left + right
	override fun computeFloat(left: Double, right: Double): Double = left + right
}

class SubtractNode(left: IRNode, right: IRNode, type: NumberType) : BinaryMathOperatorNode(left, right, type) {
	override fun computeInt(left: Int, right: Int): Int = left - right
	override fun computeFloat(left: Double, right: Double): Double = left - right
}

class MultiplyNode(left: IRNode, right: IRNode, type: NumberType) : BinaryMathOperatorNode(left, right, type) {
	override fun computeInt(left: Int, right: Int): Int = left * right	
	override fun computeFloat(left: Double, right: Double): Double = left * right
}

class DivideNode(numer: IRNode, denom: IRNode, type: NumberType) : BinaryMathOperatorNode(numer, denom, type) {
	override fun computeInt(left: Int, right: Int): Int = left / right	
	override fun computeFloat(left: Double, right: Double): Double = left / right
}

class ExponentNode(base: IRNode, exponent: IRNode, type: NumberType) : BinaryMathOperatorNode(base, exponent, type) {
	override fun computeInt(left: Int, right: Int): Int = Math.pow(left.toDouble(), right.toDouble()).toInt()
	override fun computeFloat(left: Double, right: Double): Double = Math.pow(left, right)
}