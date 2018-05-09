package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

val BUILTIN_MAP_TYPE = MapType(TypeVariable(), TypeVariable())

const val MAP_REMOVE_METHOD = "remove"
const val MAP_SIZE_METHOD = "size"
const val MAP_KEYS_METHOD = "keys"
const val MAP_VALUES_METHOD = "values"
const val MAP_CONTAINS_KEY_METHOD = "containsKey"
const val MAP_CONTAINS_VALUE_METHOD = "containsValue"
const val MAP_TO_STRING_METHOD = "toString"

val MAP_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    MAP_REMOVE_METHOD to MapRemoveBuiltinMethod(),
    MAP_SIZE_METHOD to MapSizeBuiltinMethod(),
    MAP_KEYS_METHOD to MapKeysBuiltinMethod(),
    MAP_VALUES_METHOD to MapValuesBuiltinMethod(),
    MAP_CONTAINS_KEY_METHOD to MapContainsKeyBuiltinMethod(),
    MAP_CONTAINS_VALUE_METHOD to MapContainsValueBuiltinMethod(),
    MAP_TO_STRING_METHOD to MapToStringBuiltinMethod()
)

/**
 * A builtin which removes a key and its associated value from a map.
 */
class MapRemoveBuiltinMethod(
) : BuiltinMethod(
    MAP_REMOVE_METHOD,
    FunctionType(listOf(BUILTIN_MAP_TYPE.keyType), UnitType),
    BUILTIN_MAP_TYPE
) {
    /**
    * Remove a key and its associated value from a map.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        val newItem = args[0]

        receiver.map.remove(newItem)

        return UnitValue
    }
}

/**
 * A builtin which returns the size of a map.
 */
class MapSizeBuiltinMethod(
) : BuiltinMethod(
    MAP_SIZE_METHOD,
    FunctionType(listOf(), IntType),
    BUILTIN_MAP_TYPE
) {
    /**
    * Return the size of a map.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        return IntValue(receiver.map.size)
    }
}

/**
 * A builtin which returns a set of all keys in a map.
 */
class MapKeysBuiltinMethod(
) : BuiltinMethod(
    MAP_KEYS_METHOD,
    FunctionType(listOf(), SetType(BUILTIN_MAP_TYPE.keyType)),
    BUILTIN_MAP_TYPE
) {
    /**
    * Return the set of all keys in a map.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        val receiverType = recv.type as MapType
        return SetValue(receiver.map.keys, SetType(receiverType.keyType))
    }
}

/**
 * A builtin which returns a set of all values in a map.
 */
class MapValuesBuiltinMethod(
) : BuiltinMethod(
    MAP_VALUES_METHOD,
    FunctionType(listOf(), VectorType(BUILTIN_MAP_TYPE.valType)),
    BUILTIN_MAP_TYPE
) {
    /**
    * Return the set of all values in a map.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        val receiverType = recv.type as MapType
        return VectorValue(receiver.map.values.toMutableList(), VectorType(receiverType.valType))
    }
}

/**
 * A builtin which returns whether a map contains a key.
 */
class MapContainsKeyBuiltinMethod(
) : BuiltinMethod(
    MAP_CONTAINS_KEY_METHOD,
    FunctionType(listOf(BUILTIN_MAP_TYPE.keyType), BoolType),
    BUILTIN_MAP_TYPE
) {
    /**
    * Returns whether a map contains a key.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        val item = args[0]

        return BoolValue(receiver.map.containsKey(item))
    }
}

/**
 * A builtin which returns whether a map contains a value.
 */
class MapContainsValueBuiltinMethod(
) : BuiltinMethod(
    MAP_CONTAINS_VALUE_METHOD,
    FunctionType(listOf(BUILTIN_MAP_TYPE.valType), BoolType),
    BUILTIN_MAP_TYPE
) {
    /**
    * Returns whether a map contains a value.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        val item = args[0]

        return BoolValue(receiver.map.containsValue(item))
    }
}

/**
 * A builtin which converts a map to a string.
 */
class MapToStringBuiltinMethod(
) : BuiltinMethod(
    MAP_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    BUILTIN_MAP_TYPE
) {
    /**
    * Converts a map to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as MapValue
        return StringValue(receiver.toString())
    }
}

