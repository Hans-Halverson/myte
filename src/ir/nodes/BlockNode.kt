package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a block with a new lexical scope.
 *
 * @property nodes all nodes that exist within the new scope
 */
data class BlockNode(val nodes: List<IRNode>) : IRNode(UnitTypeExpression) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        nodes.map { node -> node.map(func) }
    }
}
