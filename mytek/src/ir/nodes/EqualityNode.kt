package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an equality comparison between two expressions.
 *
 * @property left the left hand side of the equality comparison
 * @property right the right hand side of the equality comparison
 */
sealed class EqualityNode(var left: IRNode, var right: IRNode) : IRNode(left.startLocation) {
    
    /**
     * Return the result of comparing two values.
     */
    abstract fun compare(a: Any, b: Any): Boolean

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

class EqualsNode(left: IRNode, right: IRNode) : EqualityNode(left, right) {
    override fun compare(a: Any, b: Any) = (a == b)
}

class NotEqualsNode(left: IRNode, right: IRNode) : EqualityNode(left, right) {
    override fun compare(a: Any, b: Any) = (a != b)
}
