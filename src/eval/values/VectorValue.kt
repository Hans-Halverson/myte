package myte.eval.values

import myte.shared.*

class VectorValue(val elements: MutableList<Value>, type: VectorType) : Value(type) {
    override fun toString(): String = elements.toString()
    
    override fun equals(other: Any?): Boolean {
        if (other !is VectorValue) {
            return false
        }

        return elements == other.elements
    }
}
