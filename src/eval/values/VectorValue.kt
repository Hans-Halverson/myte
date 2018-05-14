package myte.eval.values

import myte.shared.*

class VectorValue(val elements: MutableList<Value>, type: VectorType) : Value(type) {
    override fun toString(): String {
        return elements.joinToString(", ", "[", "]")
    }
    
    override fun equals(other: Any?): Boolean {
        if (other !is VectorValue) {
            return false
        }

        return elements == other.elements
    }

    override fun hashCode(): Int = elements.hashCode()
}
