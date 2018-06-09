package myte.ir.nodes

import myte.eval.builtins.*
import myte.shared.*

/**
 * A node that represents a builtin function call on a list of arguments.
 *
 * @property builtin the builtin function that is being called
 * @property args the expressions that evaluate to the arguments of the builtin function call
 */
class BuiltinNode(
    val builtin: Builtin,
    val args: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        args.map { arg -> arg.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is BuiltinNode) {
            return false
        }

        return (builtin == other.builtin && args == other.args)
    }
}

/**
 * A node that represents a builtin method call on a receiver and a list of arguments.
 *
 * @property builtin the builtin function that is being called
 * @property recv the receiver of the builtin method call
 * @property args the expressions that evaluate to the arguments of the builtin function call
 */
class BuiltinMethodNode(
    val builtin: BuiltinMethod,
    val recv: IRNode,
    val args: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        recv.map(func)
        args.map { arg -> arg.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is BuiltinNode) {
            return false
        }

        return (builtin == other.builtin && args == other.args)
    }
}
