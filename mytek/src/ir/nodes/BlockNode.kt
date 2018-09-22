package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a block with a new lexical scope.
 *
 * @property nodes all nodes that exist within the new scope
 * @property isExpression whether this block should be treated like an expression
 */
class BlockNode(
    var nodes: List<IRNode>,
    val isExpression: Boolean,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        nodes.forEach { node -> node.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        nodes = nodes.map(func)
        nodes.forEach { node -> node.map(func) }
    }
}
