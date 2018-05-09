package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val INT_TO_FLOAT_METHOD = "toFloat"
const val INT_TO_STRING_METHOD = "toString"

val INT_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    INT_TO_FLOAT_METHOD to IntToFloatBuiltinMethod(),
    INT_TO_STRING_METHOD to IntToStringBuiltinMethod()
)

/**
 * A builtin which converts an int to a float.
 */
class IntToFloatBuiltinMethod(
) : BuiltinMethod(
    INT_TO_FLOAT_METHOD,
    FunctionType(listOf(), FloatType),
    IntType
) {
    /**
    * Converts an int to a float.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as IntValue
        return FloatValue(receiver.num.toDouble())
    }
}

/**
 * A builtin which converts an int to a string.
 */
class IntToStringBuiltinMethod(
) : BuiltinMethod(
    INT_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    IntType
) {
    /**
    * Converts an int to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as IntValue
        return StringValue(receiver.toString())
    }
}
