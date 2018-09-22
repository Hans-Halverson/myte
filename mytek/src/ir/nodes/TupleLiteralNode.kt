package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a tuple literal.
 *
 * @property elements the elements in the tuple literal
 */
class TupleLiteralNode(
    var elements: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        elements.forEach { element -> element.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        elements = elements.map(func)
        elements.forEach { element -> element.map(func) }
    }
}
