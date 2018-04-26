package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a block with a new lexical scope.
 *
 * @property nodes all nodes that exist within the new scope
 * @property isExpression whether this block should be treated like an expression
 */
class BlockNode(
    val nodes: List<IRNode>,
    val isExpression: Boolean,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        nodes.map { node -> node.map(func) }
    }
}
