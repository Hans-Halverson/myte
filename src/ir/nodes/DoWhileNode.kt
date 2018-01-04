package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a do while loop.
 *
 * @property cond the condition of the do while loop
 * @property body the body of the do while loop
 */
class DoWhileNode(
    val cond: IRNode,
    val body: IRNode,
    startContext: Context
) : IRNode(startContext) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        cond.map(func)
        body.map(func)
    }
}
