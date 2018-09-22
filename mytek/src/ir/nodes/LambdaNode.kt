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
    var body: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        body.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        body = func(body)
        body.map(func)
    }
}
