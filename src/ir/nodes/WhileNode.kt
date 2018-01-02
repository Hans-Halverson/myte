package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a while loop.
 *
 * @property cond the condition of the while loop
 * @property body the body of the while loop
 */
data class WhileNode(val cond: IRNode, val body: IRNode) : IRNode() {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        cond.map(func)
        body.map(func)
    }
}
