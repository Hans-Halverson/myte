package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a lambda expression.
 *
 * @property formalArgs a list of identifiers for the formal arguments to the lambda expression
 * @property body the body of the lambda expression
 */
class LambdaNode(
    val formalArgs: List<Identifier>,
    val body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        body.map(func)
    }
}
