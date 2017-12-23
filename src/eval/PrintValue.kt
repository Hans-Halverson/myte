package myte.eval

import myte.eval.values.*

/**
 * Print the value to standard output if the value is not unit.
 */
fun printValue(value: Value) {
    if (value !is UnitValue) {
        println(value)
    }
}
