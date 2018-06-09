package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val STRING_SIZE_METHOD = "string.size"

/**
 * A builtin which returns the length of a string.
 */
class StringSizeBuiltinMethod(
) : BuiltinMethod(
    STRING_SIZE_METHOD,
    FunctionType(listOf(), IntType),
    StringType
) {
    /**
    * Returns the length of a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as StringValue
        return IntValue(receiver.str.length)
    }
}
