package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val FLOAT_TO_INT_METHOD = "toInt"
const val FLOAT_TO_STRING_METHOD = "toString"

val FLOAT_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    FLOAT_TO_INT_METHOD to FloatToIntBuiltinMethod(),
    FLOAT_TO_STRING_METHOD to FloatToStringBuiltinMethod()
)

/**
 * A builtin which converts a float to an int.
 */
class FloatToIntBuiltinMethod(
) : BuiltinMethod(
    FLOAT_TO_INT_METHOD,
    FunctionType(listOf(), IntType),
    FloatType
) {
    /**
    * Converts a float to an int.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as FloatValue
        return IntValue(receiver.num.toInt())
    }
}

/**
 * A builtin which converts a float to a string.
 */
class FloatToStringBuiltinMethod(
) : BuiltinMethod(
    FLOAT_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    FloatType
) {
    /**
    * Converts a float to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as FloatValue
        return StringValue(receiver.toString())
    }
}
