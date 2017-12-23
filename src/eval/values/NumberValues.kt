package myte.eval.values

import myte.shared.*

sealed class NumberValue(type: Type) : Value(type)

data class IntValue(val num: Int) : NumberValue(IntType) {
    override fun toString(): String = num.toString()
}

data class FloatValue(val num: Double) : NumberValue(FloatType) {
    override fun toString(): String = num.toString()
}
