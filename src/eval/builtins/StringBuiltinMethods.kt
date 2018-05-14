package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val STRING_SIZE_METHOD = "size"

val STRING_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    STRING_SIZE_METHOD to StringSizeBuiltinMethod(),
    TO_STRING_METHOD to StringToStringBuiltinMethod()
)

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

/**
 * A builtin which converts a string to a string.
 */
class StringToStringBuiltinMethod(
) : BuiltinMethod(
    TO_STRING_METHOD,
    TO_STRING_TYPE,
    StringType
) {
    /**
    * Converts a string to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as StringValue
        return receiver
    }
}
