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
    var args: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        args.forEach { arg -> arg.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        args = args.map(func)
        args.forEach { arg -> arg.map(func) }
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
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        recv.forEach(func)
        args.forEach { arg -> arg.forEach(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is BuiltinNode) {
            return false
        }

        return (builtin == other.builtin && args == other.args)
    }
}
