package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents the logical or of two bool expressions.
 *
 * @property left the left hand side of the logical or expression
 * @property right the right hand side of the logical or expression
 */
data class LogicalOrNode(val left: IRNode, val right: IRNode) : IRNode(BoolType) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        left.map(func)
        right.map(func)
    }
}
