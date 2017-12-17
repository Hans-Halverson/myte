package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

val FLOAT_TO_INT_BUILTIN = "floatToInt"

class FloatToIntBuiltin() : Builtin(FLOAT_TO_INT_BUILTIN, FunctionType(listOf(FloatType), IntType)) {
	override fun eval(args: List<Value>): Value {
		val float = args[0]
		if (float is FloatValue) {
			return IntValue(float.num.toInt())
		} else {
			return UnitValue
		}
	}
}
