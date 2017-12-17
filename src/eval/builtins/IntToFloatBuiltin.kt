package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

val INT_TO_FLOAT_BUILTIN = "intToFloat"

class IntToFloatBuiltin() : Builtin(INT_TO_FLOAT_BUILTIN, FunctionType(listOf(IntType), FloatType)) {
	override fun eval(args: List<Value>): Value {
		val int = args[0]
		if (int is IntValue) {
			return FloatValue(int.num.toDouble())
		} else {
			return UnitValue
		}
	}
}
