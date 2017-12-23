package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val INT_TO_STRING_BUILTIN = "intToString"

/**
 * A builtin which converts a single int to a string.
 */
class IntToStringBuiltin(
) : Builtin(INT_TO_STRING_BUILTIN, FunctionType(listOf(IntType), StringType)) {
    
    /**
     * Convert a single int to a string.
     */
    override fun eval(args: List<Value>): Value {
        val int = args[0]
        if (int is IntValue) {
            return StringValue(int.toString())
        } else {
            return UnitValue
        }
    }
}
