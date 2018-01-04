package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a return statement.
 *
 * @property expr the (optional) expression whose value is returned
 * @property returnContext the context for the return keyword
 */
data class ReturnNode(val expr: IRNode?, val returnContext: Context) : IRNode(returnContext) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr?.map(func)
    }
}
