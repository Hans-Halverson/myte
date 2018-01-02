package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a for loop.
 *
 * @property init the (optional) initial statement that is evaluated before the for loop begins
 * @property cond the (optional) condition of the for loop
 * @property update the (optional) statement that is evaluated at the end of every loop
 * @property body the body of the for loop
 */
data class ForNode(
    val init: IRNode?,
    val cond: IRNode?,
    val update: IRNode?,
    val body: IRNode
) : IRNode() {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        init?.map(func)
        cond?.map(func)
        update?.map(func)
        body.map(func)
    }
}
