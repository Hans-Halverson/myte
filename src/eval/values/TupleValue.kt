package myte.eval.values

import myte.shared.*

class TupleValue(val tuple: MutableList<Value>, type: TupleType) : Value(type) {
    override fun toString(): String = tuple.joinToString(", ", "(", ")")
    
    override fun equals(other: Any?): Boolean {
        if (other !is TupleValue) {
            return false
        }

        return tuple == other.tuple
    }

    override fun hashCode(): Int = tuple.hashCode()
}
