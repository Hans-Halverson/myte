package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an assignment through pattern matching deconstruction.
 *
 * @property pattern the pattern containing variables to be reassigned
 * @property rValue the expression whose value is assigned to the deconstructed pattern
 * @property startLocation the location of the start of the pattern
 */
class PatternAssignmentNode(
    val pattern: IRNode,
    val rValue: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        pattern.map(func)
        rValue.map(func)
    }

    override fun equals(other: Any?): Boolean {
        if (other !is PatternAssignmentNode) {
            return false
        }

        return (pattern == other.pattern && rValue == other.rValue)
    }
}
