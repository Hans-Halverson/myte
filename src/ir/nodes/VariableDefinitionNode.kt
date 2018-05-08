package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a variable definition.
 *
 * @property ident the identifier of the new variable
 * @property expr the expression whose value is bound to the new variable
 * @property typeAnnotation an optional type annotation
 * @property identLocation the location for the identifier that is defined in this node
 */
class VariableDefinitionNode(
    val ident: Identifier,
    val expr: IRNode,
    val typeAnnotation: Type?,
    val identLocation: Location,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        expr.map(func)
    }

    override fun toString(): String {
        return "VariableDefinitionNode(ident=${ident}, expr=${expr})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is VariableDefinitionNode) {
            return false
        }

        return (ident == other.ident && expr == other.expr)
    }
}

