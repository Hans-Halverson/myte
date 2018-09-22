package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a map literal.
 *
 * @property keys the keys in the map literal, one for each value
 * @property values the values in the map literal, one for each key
 */
class MapLiteralNode(
    var keys: List<IRNode>,
    var values: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        keys.forEach { key -> key.forEach(func) }
        values.forEach { value -> value.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        keys = keys.map(func)
        values = values.map(func)

        keys.forEach { key -> key.map(func) }
        values.forEach { value -> value.map(func) }
    }
}
