package myte.eval.builtins

import myte.eval.*
import myte.eval.values.*
import myte.shared.*

const val TO_STRING_METHOD = "toString"
val TO_STRING_TYPE = FunctionType(listOf(), StringType)

fun callToString(value: Value, env: Environment, eval: Evaluator): StringValue {
    val toStringFunction = eval.accessValue(value, TO_STRING_METHOD, TO_STRING_TYPE,
            NO_LOCATION, env)
    return eval.applyFunction(toStringFunction, listOf(), env, NO_LOCATION) as StringValue
}
