package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a variable being reassigned a new value.
 *
 * @property lValue the identifier of the variable that is being reassigned
 * @property rValue the expression whose value is assigned to the variable
 * @property identLocation the location for the identifier of the variable
 */
class VariableAssignmentNode(
    val lValue: Identifier,
    var rValue: IRNode,
    val identLocation: Location
) : IRNode(identLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        rValue.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        rValue = func(rValue)
        rValue.map(func)
    }

    override fun equals(other: Any?): Boolean {
        if (other !is VariableAssignmentNode) {
            return false
        }

        return (lValue == other.lValue && rValue == other.rValue)
    }
}
