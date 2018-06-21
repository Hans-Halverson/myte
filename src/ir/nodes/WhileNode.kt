package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a while loop.
 *
 * @property cond the condition of the while loop
 * @property body the body of the while loop
 */
class WhileNode(
    var cond: IRNode,
    var body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        cond.forEach(func)
        body.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        cond = func(cond)
        body = func(body)

        cond.map(func)
        body.map(func)
    }
}
