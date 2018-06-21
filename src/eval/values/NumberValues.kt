package myte.eval.values

import myte.shared.*

sealed class NumberValue(type: Type) : Value(type)

data class ByteValue(val num: Byte) : NumberValue(ByteType) {
    override fun toString(): String = num.toString()
}

data class IntValue(val num: Int) : NumberValue(IntType) {
    override fun toString(): String = num.toString()
}

data class FloatValue(val num: Float) : NumberValue(FloatType) {
    override fun toString(): String = num.toString()
}

data class DoubleValue(val num: Double) : NumberValue(DoubleType) {
    override fun toString(): String = num.toString()
}

