package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents the logical and of two bool expressions.
 *
 * @property left the left hand side of the logical and expression
 * @property right the right hand side of the logical and expression
 */
data class LogicalAndNode(val left: IRNode, val right: IRNode) : IRNode(left.startContext) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        left.map(func)
        right.map(func)
    }
}
