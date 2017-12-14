package myte.eval

import myte.eval.values.*

fun stringOfValue(value: Value): String? {
	return when (value) {
		is BooleanValue -> if (value.bool) "true" else "false"
		is NumberValue -> value.num.toString()
		is Closure -> "${value.ident.name}: ${value.type}"
		else -> null
	}
}

fun printValue(value: Value) {
	val str = stringOfValue(value)
	if (str != null) {
		println(stringOfValue(value))
	}
}
