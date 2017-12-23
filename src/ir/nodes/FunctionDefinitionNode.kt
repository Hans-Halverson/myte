package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a function definition.
 *
 * @property ident the identifier of the function that is being defined
 * @property formalArgs a list of identifiers for the formal arguments to the function
 * @body the body of the function
 */
data class FunctionDefinitionNode(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    val body: IRNode
) : IRNode(UnitTypeExpression) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        body.map(func)
    }
}
