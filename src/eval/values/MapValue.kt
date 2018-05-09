package myte.eval.values

import myte.shared.*

class MapValue(val map: MutableMap<Value, Value>, type: MapType) : Value(type) {
    override fun toString(): String {
        // Return special string for empty map
        if (map.size == 0) {
            return "[||]"
        }

        val builder = StringBuilder()
        builder.append("[|")

        var firstPair = true
        for ((key, value) in map) {
            // Add a comma separator between every two kvpairs
            if (firstPair) {
                firstPair = false
            } else {
                builder.append(", ")
            }

            // Add the pair itself to the string, formatted as key -> value
            builder.append(key)
            builder.append(" -> ")
            builder.append(value)
        }

        builder.append("|]")

        return builder.toString()
    }

    
    override fun equals(other: Any?): Boolean {
        if (other !is MapValue) {
            return false
        }

        return map == other.map
    }

    override fun hashCode(): Int = map.hashCode()
}
