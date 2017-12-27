package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a tuple literal.
 *
 * @property elements the elements in the tuple literal
 */
class TupleLiteralNode(
    val elements: List<IRNode>
) : IRNode(TupleTypeExpression(elements.map { newTypeVariable() })) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        elements.map { element -> element.map(func) }
    }
}
