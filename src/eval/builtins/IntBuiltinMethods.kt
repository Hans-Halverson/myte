package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val INT_TO_FLOAT_METHOD = "int.toFloat"
const val INT_TO_STRING_METHOD = "int.toString"

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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    TO_STRING_TYPE,
    IntType
) {
    /**
    * Converts an int to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return StringValue(receiver.toString())
    }
}
