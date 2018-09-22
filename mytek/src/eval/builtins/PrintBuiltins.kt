package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

const val PRINT_BUILTIN = "__print"
const val PRINT_LINE_BUILTIN = "__println"

/**
 * A builtin which prints out a single string without a newline.
 */
class PrintBuiltin(
) : Builtin(PRINT_BUILTIN, FunctionType(listOf(StringType), UnitType)) {
    
    /**
    * Print out a single string without a newline.
    */
    override fun eval(args: List<Value>): Value {
        val str = args[0]
        if (str is StringValue) {
            print(str.str)
        }
        
        return UnitValue
    }
}

/**
 * A builtin which prints out a single string with a newline.
 */
class PrintLineBuiltin(
) : Builtin(PRINT_LINE_BUILTIN, FunctionType(listOf(StringType), UnitType)) {
    
    /**
    * Print out a single string with a newline.
    */
    override fun eval(args: List<Value>): Value {
        val str = args[0]
        if (str is StringValue) {
            println(str.str)
        }
        
        return UnitValue
    }
}
