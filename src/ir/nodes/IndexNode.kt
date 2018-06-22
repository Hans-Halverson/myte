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
    var container: IRNode,
    var key: IRNode,
    val indexLocation: Location
) : IRNode(container.startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        container.forEach(func)
        key.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        container = func(container)
        key = func(key)

        container.map(func)
        key.map(func)
    }
}
