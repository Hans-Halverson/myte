package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a function definition.
 *
 * @property ident the identifier of the function that is being defined
 * @property formalArgs a list of identifiers for the formal arguments to the function
 * @property body the body of the function
 * @property identContext the context for the identifier of hte function
 */
class FunctionDefinitionNode(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    val body: IRNode,
    val identContext: Context,
    startContext: Context
) : IRNode(startContext) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        body.map(func)
    }
}
