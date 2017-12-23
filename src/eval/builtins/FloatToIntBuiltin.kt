package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val FLOAT_TO_INT_BUILTIN = "floatToInt"

/**
 * A builtin which converts a single float to an int.
 */
class FloatToIntBuiltin(
) : Builtin(FLOAT_TO_INT_BUILTIN, FunctionType(listOf(FloatType), IntType)) {
    
    /**
    * Convert a single float to an int.
    */
    override fun eval(args: List<Value>): Value {
        val float = args[0]
        if (float is FloatValue) {
            return IntValue(float.num.toInt())
        } else {
            return UnitValue
        }
    }
}
