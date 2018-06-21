package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a field assignment.
 *
 * @property expr the expression whose result will have a field assigned
 * @property field the field that is being assigned
 * @property rValue the value to assign to the given field
 * @property accessLocation the location of the dot in the access
 * */
data class FieldAssignmentNode(
    var expr: IRNode,
    val field: String,
    var rValue: IRNode,
    val accessLocation: Location
) : IRNode(expr.startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        expr.forEach(func)
        rValue.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        expr = func(expr)
        rValue = func(rValue)

        expr.map(func)
        rValue.map(func)
    }
}
