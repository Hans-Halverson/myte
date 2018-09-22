package myte.eval.values

import myte.shared.*

data class BoolValue(val bool: Boolean) : Value(BoolType) {
    override fun toString(): String = if (bool) "true" else "false"
}
