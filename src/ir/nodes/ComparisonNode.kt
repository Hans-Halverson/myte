package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a comparison between two numbers.
 *
 * @property left the left hand side of the comparison
 * @property right the right hand side of the comparison
 */
sealed class ComparisonNode(val left: IRNode, val right: IRNode) : IRNode(left.startLocation) {

    /**
     * Return the result of comparing two ints.
     */
    abstract fun compareInts(a: Int, b: Int): Boolean

    /**
     * Return the result of comparing two floats.
     */
    abstract fun compareFloats(a: Double, b: Double): Boolean

    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        left.map(func)
        right.map(func)
    }
}

class LessThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareInts(a: Int, b: Int) = (a < b)

    override fun compareFloats(a: Double, b: Double) = (a < b)
}

class LessThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareInts(a: Int, b: Int) = (a <= b)

    override fun compareFloats(a: Double, b: Double) = (a <= b)
}

class GreaterThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareInts(a: Int, b: Int) = (a > b)

    override fun compareFloats(a: Double, b: Double) = (a > b)
}

class GreaterThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareInts(a: Int, b: Int) = (a >= b)
    
    override fun compareFloats(a: Double, b: Double) = (a >= b)
}
