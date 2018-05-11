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
    val expr: IRNode,
    val field: String,
    val rValue: IRNode,
    val accessLocation: Location
) : IRNode(expr.startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        rValue.map(func)
    }
}
