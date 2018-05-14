package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

val TUPLE_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    TO_STRING_METHOD to TupleToStringBuiltinMethod()
)

/**
 * A builtin which converts a tuple to a string.
 */
class TupleToStringBuiltinMethod(
) : BuiltinMethod(
    TO_STRING_METHOD,
    TO_STRING_TYPE,
    UnitType
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
