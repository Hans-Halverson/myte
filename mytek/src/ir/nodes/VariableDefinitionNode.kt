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
    var expr: IRNode,
    val typeAnnotation: Type?,
    val identLocation: Location,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        expr.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        expr = func(expr)
        expr.map(func)
    }

    override fun equals(other: Any?): Boolean {
        if (other !is VariableDefinitionNode) {
            return false
        }

        return (ident == other.ident && expr == other.expr)
    }
}

