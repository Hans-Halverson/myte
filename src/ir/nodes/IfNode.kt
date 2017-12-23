package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents an if statement.
 *
 * @property cond the condition of the if statement
 * @property conseq the branch of the if statement that is evaluated if the condition is true
 * @property altern the (optional) branch of the if statement that is evaluated if
 *           the condition is false
 */
data class IfNode(
    val cond: IRNode,
    val conseq: IRNode,
    val altern: IRNode?
) : IRNode(UnitTypeExpression) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        cond.map(func)
        conseq.map(func)
        altern?.map(func)
    }
}
