package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a vector literal.
 *
 * @property elements the elements in the vector literal
 */
class VectorLiteralNode(
    val elements: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        elements.map { element -> element.map(func) }
    }
}
