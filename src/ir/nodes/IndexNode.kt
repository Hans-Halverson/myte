package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an index into a container.
 *
 * @property container the container that is being indexed into
 * @property key the key used to index into the container
 * @property accessLocation the location of the left bracket in the access
 */
data class IndexNode(
    val container: IRNode,
    val key: IRNode,
    val indexLocation: Location
) : IRNode(container.startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        container.map(func)
        key.map(func)
    }
}
