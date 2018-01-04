package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents the logical not of a bool expression.
 *
 * @property node the bool expression
 */
class LogicalNotNode(val node: IRNode, startContext: Context) : IRNode(startContext) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        node.map(func)
    }
}
