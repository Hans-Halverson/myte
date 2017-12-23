package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val INT_TO_FLOAT_BUILTIN = "intToFloat"

/**
 * A builtin which converts a single int to a float.
 */
class IntToFloatBuiltin(
) : Builtin(INT_TO_FLOAT_BUILTIN, FunctionType(listOf(IntType), FloatType)) {
    
    /**
     * Convert a single int to a float.
     */
    override fun eval(args: List<Value>): Value {
        val int = args[0]
        if (int is IntValue) {
            return FloatValue(int.num.toDouble())
        } else {
            return UnitValue
        }
    }
}
