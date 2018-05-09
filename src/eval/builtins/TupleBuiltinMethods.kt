package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

val BUILTIN_TUPLE_TYPE = TupleType(listOf())

const val TUPLE_TO_STRING_METHOD = "toString"

val TUPLE_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    TUPLE_TO_STRING_METHOD to TupleToStringBuiltinMethod()
)

/**
 * A builtin which converts a tuple to a string.
 */
class TupleToStringBuiltinMethod(
) : BuiltinMethod(
    TUPLE_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    BUILTIN_TUPLE_TYPE
) {
    /**
    * Converts a tuple to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as TupleValue
        return StringValue(receiver.toString())
    }
}
