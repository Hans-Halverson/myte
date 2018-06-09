package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val FLOAT_TO_INT_METHOD = "float.toInt"
const val FLOAT_TO_STRING_METHOD = "float.toString"

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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    TO_STRING_TYPE,
    FloatType
) {
    /**
    * Converts a float to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return StringValue(receiver.toString())
    }
}
