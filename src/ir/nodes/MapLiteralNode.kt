package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a map literal.
 *
 * @property keys the keys in the map literal, one for each value
 * @property values the values in the map literal, one for each key
 */
class MapLiteralNode(
    val keys: List<IRNode>,
    val values: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        keys.map { key -> key.map(func) }
        values.map { value -> value.map(func) }
    }
}
