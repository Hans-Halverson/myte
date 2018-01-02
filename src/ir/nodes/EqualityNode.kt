package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an equality comparison between two expressions.
 *
 * @property left the left hand side of the equality comparison
 * @property right the right hand side of the equality comparison
 */
sealed class EqualityNode(val left: IRNode, val right: IRNode) : IRNode() {
    
    /**
     * Return the result of comparing two values.
     */
    abstract fun compare(a: Any, b: Any): Boolean

    override fun <T> map(func: (IRNode) -> T) {
        func(this)
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
