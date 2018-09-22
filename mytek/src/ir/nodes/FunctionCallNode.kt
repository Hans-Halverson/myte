package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a function call on a list of arguments.
 *
 * @property func the node that evaluates to the function that is being called
 * @property actualArgs the expressions that evaluate to the arguments of the function call
 * @property callLocation the location of the actual function call (pointing to the left paren)
 */
class FunctionCallNode(
    var func: IRNode,
    var actualArgs: List<IRNode>,
    val callLocation: Location,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        this.func.forEach(func)
        actualArgs.forEach { arg -> arg.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        this.func = func(this.func)
        actualArgs = actualArgs.map(func)

        this.func.map(func)
        actualArgs.forEach { arg -> arg.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is FunctionCallNode) {
            return false
        }

        return (func == other.func && actualArgs == other.actualArgs)
    }
}
