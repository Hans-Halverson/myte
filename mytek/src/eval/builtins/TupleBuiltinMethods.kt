package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val TUPLE_TO_STRING_METHOD = "__tuple.toString"

/**
 * A builtin which converts a tuple to a string.
 */
class TupleToStringBuiltinMethod(
) : BuiltinMethod(
    TUPLE_TO_STRING_METHOD,
    TO_STRING_TYPE,
    TupleType(listOf())
) {
    /**
    * Converts a tuple to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as TupleValue

        val elementStrings: List<String> = receiver.tuple.map { value ->
            callToString(value, env, eval).str
        }

        return StringValue(elementStrings.joinToString(", ", "(", ")"))
    }
}
