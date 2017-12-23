package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a variable being reassigned a new value.
 *
 * @property ident the identifier of the variable that is being reassigned
 * @property expr the expression whose value is assigned to the variable
 */
class AssignmentNode(
    val ident: Identifier,
    val expr: IRNode,
    evalTypeExpr: TypeExpression
) : IRNode(evalTypeExpr) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr.map(func)
    }

    override fun toString(): String {
        return "AssignmentNode(ident=${ident}, expr=${expr})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is AssignmentNode) {
            return false
        }

        return (ident == other.ident && expr == other.expr)
    }
}
