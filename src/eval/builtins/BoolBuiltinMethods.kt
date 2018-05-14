package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*


val BOOL_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    TO_STRING_METHOD to BoolToStringBuiltinMethod()
)

/**
 * A builtin which converts a bool to a string.
 */
class BoolToStringBuiltinMethod(
) : BuiltinMethod(
    TO_STRING_METHOD,
    TO_STRING_TYPE,
    BoolType
) {
    /**
    * Converts a bool to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as BoolValue
        return StringValue(receiver.toString())
    }
}
