package myte.eval.values

import myte.shared.*

data class StringValue(val str: String) : Value(StringType) {
    override fun toString(): String = "\"${str}\""
}
