package myte.eval

import myte.eval.values.*
import myte.shared.*

/**
 * Print the value and its type to standard output if the value is not unit.
 */
fun printValue(value: Value) {
    if (value !is UnitValue) {
        println("${value} : ${formatType(value.type)}")
    }
}
