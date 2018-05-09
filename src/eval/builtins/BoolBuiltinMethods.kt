package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val BOOL_TO_STRING_METHOD = "toString"

val BOOL_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    BOOL_TO_STRING_METHOD to BoolToStringBuiltinMethod()
)

/**
 * A builtin which converts a bool to a string.
 */
class BoolToStringBuiltinMethod(
) : BuiltinMethod(
    BOOL_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    BoolType
) {
    /**
    * Converts a bool to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as BoolValue
        return StringValue(receiver.toString())
    }
}
