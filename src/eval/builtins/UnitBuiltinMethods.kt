package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val UNIT_TO_STRING_METHOD = "toString"

val UNIT_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    UNIT_TO_STRING_METHOD to UnitToStringBuiltinMethod()
)

/**
 * A builtin which converts a unit to a string.
 */
class UnitToStringBuiltinMethod(
) : BuiltinMethod(
    UNIT_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    UnitType
) {
    /**
    * Converts a unit to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as UnitValue
        return StringValue(receiver.toString())
    }
}
