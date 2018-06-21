package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a unary arithmetic operator applied to an expression.
 *
 * @property node the arithmetic expression the operation is applied to
 */
sealed class UnaryMathOperatorNode(
    var node: IRNode,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        node.forEach(func)
    }

    override fun map(func: (IRNode) -> IRNode) {
        node = func(node)
        node.map(func)
    }
}

/**
 * A node that has no effect on the expression contained in it.
 */
class IdentityNode(
    node: IRNode,
    startLocation: Location
) : UnaryMathOperatorNode(node, startLocation)

/**
 * A node that negates the expression contained in it.
 */
class NegateNode(
    node: IRNode,
    startLocation: Location
) : UnaryMathOperatorNode(node, startLocation)
