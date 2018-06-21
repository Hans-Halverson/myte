package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a return statement.
 *
 * @property expr the (optional) expression whose value is returned
 * @property returnLocation the location for the return keyword
 */
data class ReturnNode(var expr: IRNode?, val returnLocation: Location) : IRNode(returnLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        expr?.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        expr = expr?.let { func(it) }
        expr?.map(func)
    }
}
