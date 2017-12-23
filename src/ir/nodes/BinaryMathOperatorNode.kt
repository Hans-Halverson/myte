package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an infix arithmetic operator applied to two numbers.
 *
 * @property left the left hand side of the arithmetic expression
 * @property right the right hand side of the arithmetic expression
 */
sealed class BinaryMathOperatorNode(
    val left: IRNode,
    val right: IRNode
) : IRNode(newTypeVariable()) {
    
    /**
     * Return the result of applying this operation to two ints.
     */
    abstract fun computeInt(left: Int, right: Int): Int

    /**
     * Return the result of applying this operation to two floats.
     */
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
    override fun computeInt(left: Int, right: Int): Int {
        return Math.pow(left.toDouble(), right.toDouble()).toInt()
    }

    override fun computeFloat(left: Double, right: Double): Double = Math.pow(left, right)
}
