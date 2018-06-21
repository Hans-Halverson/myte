package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents the logical or of two bool expressions.
 *
 * @property left the left hand side of the logical or expression
 * @property right the right hand side of the logical or expression
 */
data class LogicalOrNode(var left: IRNode, var right: IRNode) : IRNode(left.startLocation) {
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
