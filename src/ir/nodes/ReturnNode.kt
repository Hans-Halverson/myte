package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a return statement.
 *
 * @property expr the (optional) expression whose value is returned
 */
data class ReturnNode(val expr: IRNode?) : IRNode(UnitType) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr?.map(func)
    }
}
