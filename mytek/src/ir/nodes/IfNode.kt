package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an if statement.
 *
 * @property cond the condition of the if statement
 * @property conseq the branch of the if statement that is evaluated if the condition is true
 * @property altern the (optional) branch of the if statement that is evaluated if
 *           the condition is false
 * @property isExpression whether this if should be treated like an expression
 */
class IfNode(
    var cond: IRNode,
    var conseq: IRNode,
    var altern: IRNode?,
    val isExpression: Boolean,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        cond.forEach(func)
        conseq.forEach(func)
        altern?.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        cond = func(cond)
        conseq = func(conseq)
        altern = altern?.let { func(it) }

        cond.map(func)
        conseq.map(func)
        altern?.map(func)
    }
}
