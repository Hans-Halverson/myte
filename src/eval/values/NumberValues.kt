package myte.eval.values

import myte.shared.*

sealed class NumberValue(type: Type) : Value(type)

data class IntValue(val num: Int) : NumberValue(IntType)

data class FloatValue(val num: Double) : NumberValue(FloatType)
