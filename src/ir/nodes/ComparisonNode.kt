package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a comparison between two numbers.
 *
 * @property left the left hand side of the comparison
 * @property right the right hand side of the comparison
 */
sealed class ComparisonNode(var left: IRNode, var right: IRNode) : IRNode(left.startLocation) {

    /**
     * Return the result of comparing two bytes.
     */
    abstract fun compareBytes(a: Byte, b: Byte): Boolean

    /**
     * Return the result of comparing two ints.
     */
    abstract fun compareInts(a: Int, b: Int): Boolean

    /**
     * Return the result of comparing two floats.
     */
    abstract fun compareFloats(a: Float, b: Float): Boolean

    /**
     * Return the result of comparing two doubles.
     */
    abstract fun compareDoubles(a: Double, b: Double): Boolean

    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        left.forEach(func)
        right.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        left = func(left)
        right = func(right)

        left.map(func)
        right.map(func)
    }
}

class LessThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareBytes(a: Byte, b: Byte) = (a < b)

    override fun compareInts(a: Int, b: Int) = (a < b)

    override fun compareFloats(a: Float, b: Float) = (a < b)

    override fun compareDoubles(a: Double, b: Double) = (a < b)
}

class LessThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareBytes(a: Byte, b: Byte) = (a <= b)

    override fun compareInts(a: Int, b: Int) = (a <= b)

    override fun compareFloats(a: Float, b: Float) = (a <= b)

    override fun compareDoubles(a: Double, b: Double) = (a <= b)
}

class GreaterThanNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareBytes(a: Byte, b: Byte) = (a > b)

    override fun compareInts(a: Int, b: Int) = (a > b)

    override fun compareFloats(a: Float, b: Float) = (a > b)

    override fun compareDoubles(a: Double, b: Double) = (a > b)
}

class GreaterThanOrEqualNode(left: IRNode, right: IRNode) : ComparisonNode(left, right) {
    override fun compareBytes(a: Byte, b: Byte) = (a >= b)

    override fun compareInts(a: Int, b: Int) = (a >= b)

    override fun compareFloats(a: Float, b: Float) = (a >= b)

    override fun compareDoubles(a: Double, b: Double) = (a >= b)
}
