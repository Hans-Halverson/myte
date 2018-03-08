package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an element in a container being reassigned a new value.
 *
 * @property container the container which has an element reassigned
 * @property the key of the element in the container which should be reassigned
 * @property rValue the expression whose value is assigned to that element
* @property accessLocation the location of the left bracket in the access
 */
class KeyedAssignmentNode(
    val container: IRNode,
    val key: IRNode,
    val rValue: IRNode,
    val accessLocation: Location
) : IRNode(container.startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        container.map(func)
        key.map(func)
        rValue.map(func)
    }

    override fun toString(): String {
        return "KeyedAssignmentNode(container=${container}, key=${key}, rValue=${rValue})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is KeyedAssignmentNode) {
            return false
        }

        return (container == other.container && key == other.key && rValue == other.rValue)
    }
}
