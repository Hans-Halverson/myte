package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*


val UNIT_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    TO_STRING_METHOD to UnitToStringBuiltinMethod()
)

/**
 * A builtin which converts a unit to a string.
 */
class UnitToStringBuiltinMethod(
) : BuiltinMethod(
    TO_STRING_METHOD,
    TO_STRING_TYPE,
    UnitType
) {
    /**
    * Converts a unit to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as UnitValue
        return StringValue(receiver.toString())
    }
}
