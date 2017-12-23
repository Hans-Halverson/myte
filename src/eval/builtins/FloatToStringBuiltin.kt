package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val FLOAT_TO_STRING_BUILTIN = "floatToString"

/**
 * A builtin which converts a single float to a string.
 */
class FloatToStringBuiltin(
) : Builtin(FLOAT_TO_STRING_BUILTIN, FunctionType(listOf(FloatType), StringType)) {
    
    /**
     * Convert a single float to a string.
     */
    override fun eval(args: List<Value>): Value {
        val float = args[0]
        if (float is FloatValue) {
            return StringValue(float.toString())
        } else {
            return UnitValue
        }
    }
}
