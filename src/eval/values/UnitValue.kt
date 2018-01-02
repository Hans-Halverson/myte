package myte.eval.values

import myte.shared.*

object UnitValue : Value(UnitType) {
    override fun toString(): String = "unit"
}
