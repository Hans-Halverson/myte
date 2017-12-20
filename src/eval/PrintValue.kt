package myte.eval

import myte.eval.values.*

fun printValue(value: Value) {
	if (value !is UnitValue) {
		println(value)
	}
}
