package myte.eval.values

import myte.shared.*

class ListValue(val list: MutableList<Value>, type: ListType) : Value(type) {
    override fun toString(): String = list.toString()
    
    override fun equals(other: Any?): Boolean {
        if (other !is ListValue) {
            return false
        }

        return list == other.list
    }
}
