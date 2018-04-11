package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a set literal.
 *
 * @property elements the elements in the set literal
 */
class SetLiteralNode(
    val elements: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        elements.map { element -> element.map(func) }
    }
}
