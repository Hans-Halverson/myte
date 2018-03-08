package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a return statement.
 *
 * @property expr the (optional) expression whose value is returned
 * @property returnLocation the location for the return keyword
 */
data class ReturnNode(val expr: IRNode?, val returnLocation: Location) : IRNode(returnLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr?.map(func)
    }
}
