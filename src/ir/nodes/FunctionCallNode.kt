package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a function call on a list of arguments.
 *
 * @property func the node that evaluates to the function that is being called
 * @property actualArgs the expressions that evaluate to the arguments of the function call
 */
class FunctionCallNode(
    val func: IRNode,
    val actualArgs: List<IRNode>,
    startContext: Context
) : IRNode(startContext) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        actualArgs.map { arg -> arg.map(func) }
    }

    override fun toString(): String {
        return "FunctionCallNode(func=${func}, actualArgs=${actualArgs})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is FunctionCallNode) {
            return false
        }

        return (func == other.func && actualArgs == other.actualArgs)
    }
}
