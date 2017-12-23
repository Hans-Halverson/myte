package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a unary arithmetic operator applied to an expression.
 *
 * @property node the arithmetic expression the operation is applied to
 */
sealed class UnaryMathOperatorNode(val node: IRNode) : IRNode(newTypeVariable()) {

    /**
     * Return the result of applying this operation to an int.
     */
    abstract fun computeInt(num: Int): Int

    /**
     * Return the result of applying this operation to an int.
     */
    abstract fun computeFloat(num: Double): Double

    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        node.map(func)
    }
}

/**
 * A node that has no effect on the expression contained in it.
 */
class IdentityNode(node: IRNode) : UnaryMathOperatorNode(node) {
    override fun computeInt(num: Int): Int = num

    override fun computeFloat(num: Double): Double = num
}

/**
 * A node that negates the expression contained in it.
 */
class NegateNode(node: IRNode) : UnaryMathOperatorNode(node) {
    override fun computeInt(num: Int): Int = -num
    
    override fun computeFloat(num: Double): Double = -num
}
