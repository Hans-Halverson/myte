package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a field access.
 *
 * @property expr the expression that has a field being accessed
 * @property field the field that is being accessed
 * @property accessLocation the location of the dot in the access
 */
data class AccessNode(
    var expr: IRNode,
    val field: String,
    val accessLocation: Location
) : IRNode(expr.startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        expr.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        expr = func(expr)
        expr.map(func)
    }
}
