package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a variable.
 *
 * @property ident the identifier of the variable
 */
class VariableNode(val ident: Identifier, evalTypeExpr: TypeExpression): IRNode(evalTypeExpr) {
    override fun toString(): String {
        return "VariableNode(ident=${ident})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is VariableNode) {
            return false
        }

        return ident == other.ident
    }
}

