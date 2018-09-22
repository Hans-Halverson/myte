package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents the logical not of a bool expression.
 *
 * @property node the bool expression
 */
class LogicalNotNode(var node: IRNode, startLocation: Location) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        node.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        node = func(node)
        node.map(func)
    }
}
