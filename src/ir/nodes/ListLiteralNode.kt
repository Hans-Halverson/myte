package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a list literal.
 *
 * @property elements the elements in the list literal
 */
class ListLiteralNode(val elements: List<IRNode>) : IRNode(ListTypeExpression(newTypeVariable())) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        elements.map { element -> element.map(func) }
    }
}
