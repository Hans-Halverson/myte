package myte.ir.nodes

import myte.shared.*

/**
 * A node that simply wraps another node, and defers everything to the wrapped node. This node
 * is invisible during evaluation, type checking, etc.
 *
 * @property node the wrapped node
 */
data class WrapperNode(var node: IRNode) : IRNode(NO_LOCATION) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        node.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        node = func(node)
        node.map(func)
    }
}
