package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a variable being reassigned a new value.
 *
 * @property lValue the identifier of the variable that is being reassigned
 * @property rValue the expression whose value is assigned to the variable
 */
class VariableAssignmentNode(
    val lValue: Identifier,
    val rValue: IRNode
) : IRNode() {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        rValue.map(func)
    }

    override fun toString(): String {
        return "VariableAssignmentNode(lValue=${lValue}, rValue=${rValue})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is VariableAssignmentNode) {
            return false
        }

        return (lValue == other.lValue && rValue == other.rValue)
    }
}
