package myte.eval.values

import myte.shared.*

class SetValue(val elements: MutableSet<Value>, type: SetType) : Value(type) {
    override fun toString(): String {
        return elements.joinToString(", ", "{|", "|}")
    }
    
    override fun equals(other: Any?): Boolean {
        if (other !is SetValue) {
            return false
        }

        return elements == other.elements
    }

    override fun hashCode(): Int = elements.hashCode()
}
