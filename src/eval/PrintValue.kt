package myte.eval

import myte.eval.values.*

fun stringOfValue(value: Value): String? {
	return when (value) {
		is BoolValue -> if (value.bool) "true" else "false"
		is IntValue -> value.num.toString()
		is FloatValue -> value.num.toString()
		is ClosureValue -> "${value.ident.name}: ${value.type}"
		is BuiltinValue -> "${value.ident.name}: ${value.type}"
		else -> null
	}
}

fun printValue(value: Value) {
	val str = stringOfValue(value)
	if (str != null) {
		println(stringOfValue(value))
	}
}
