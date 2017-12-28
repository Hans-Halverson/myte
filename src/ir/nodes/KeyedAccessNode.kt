package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a keyed acess into a container.
 *
 * @property container the container that is being indexed into
 * @property key the key used to index into the container
 */
data class KeyedAccessNode(val container: IRNode, val key: IRNode) : IRNode(newTypeVariable()) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        container.map(func)
        key.map(func)
    }
}
