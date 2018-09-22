package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

val BUILTIN_MAP_TYPE = MapType(TypeParameter(), TypeParameter())

const val MAP_INDEX_METHOD = "map.index"
const val MAP_INDEX_ASSIGN_METHOD = "map.indexAssign"
const val MAP_REMOVE_METHOD = "map.remove"
const val MAP_SIZE_METHOD = "map.size"
const val MAP_KEYS_METHOD = "map.keys"
const val MAP_VALUES_METHOD = "map.values"
const val MAP_CONTAINS_KEY_METHOD = "map.containsKey"
const val MAP_CONTAINS_VALUE_METHOD = "map.containsValue"
const val MAP_TO_VEC_METHOD = "map.toVec"
const val MAP_TO_STRING_METHOD = "map.toString"

val OPTION_VAL_TYPE = OPTION_TYPE_SIG.createTypeWithParams(listOf(BUILTIN_MAP_TYPE.valType))

/**
 * A builtin which returns the value corresponding to a particular key in the map.
 */
class MapIndexBuiltinMethod(
) : BuiltinMethod(
    MAP_INDEX_METHOD,
    FunctionType(listOf(BUILTIN_MAP_TYPE.keyType), OPTION_VAL_TYPE),
    BUILTIN_MAP_TYPE
) {
    /**
    * Return an option of the value corresponding to a particular key in the map.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as MapValue
        val receiverType = recv.type as MapType
        val returnType = OPTION_TYPE_SIG.createTypeWithParams(listOf(receiverType.valType))

        val value = receiver.map[args[0]]
        if (value == null) {
            val noneVariant = getVariantForBuiltinType(OPTION_TYPE_SIG, OPTION_TYPE_NONE_VARIANT)
            return TupleVariantValue(noneVariant, listOf(), returnType)
        } else {
            val someVariant = getVariantForBuiltinType(OPTION_TYPE_SIG, OPTION_TYPE_SOME_VARIANT)
            return TupleVariantValue(someVariant, listOf(value), returnType)
        }
    }
}

/**
 * A builtin which assigns the value corresponding to a particular key in the map.
 */
class MapIndexAssignBuiltinMethod(
) : BuiltinMethod(
    MAP_INDEX_ASSIGN_METHOD,
    FunctionType(listOf(BUILTIN_MAP_TYPE.keyType, BUILTIN_MAP_TYPE.valType),
            BUILTIN_MAP_TYPE.valType),
    BUILTIN_MAP_TYPE
) {
    /**
    * Assign the value corresponding to a particular key in the map.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as MapValue
        val value = args[1]
        receiver.map[args[0]] = value

        return value
    }
}

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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as MapValue
        val item = args[0]

        return BoolValue(receiver.map.containsValue(item))
    }
}

/**
 * A builtin which converts a map to a vector.
 */
class MapToVecBuiltinMethod(
) : BuiltinMethod(
    MAP_TO_VEC_METHOD,
    FunctionType(listOf(),
            VectorType(TupleType(listOf(BUILTIN_MAP_TYPE.keyType, BUILTIN_MAP_TYPE.valType)))),
    BUILTIN_MAP_TYPE
) {
    /**
    * Converts a map into a vector.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as MapValue
        val receiverType = recv.type as MapType
        val vecElementType = TupleType(listOf(receiverType.keyType, receiverType.valType))

        val vec = receiver.map.map { (key, value) ->
            TupleValue(mutableListOf(key, value), vecElementType)
        }

        return VectorValue(vec.toMutableList(), VectorType(vecElementType))
    }
}

/**
 * A builtin which converts a map to a string.
 */
class MapToStringBuiltinMethod(
) : BuiltinMethod(
    MAP_TO_STRING_METHOD,
    TO_STRING_TYPE,
    BUILTIN_MAP_TYPE
) {
    /**
    * Converts a map to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as MapValue
        val builder = StringBuilder()
        builder.append("[|")

        var firstPair = true
        for ((key, value) in receiver.map) {
            // Add a comma separator between every two kvpairs
            if (firstPair) {
                firstPair = false
            } else {
                builder.append(", ")
            }

            // Add the pair itself to the string, formatted as key -> value
            builder.append(callToString(key, env, eval).str)
            builder.append(" -> ")
            builder.append(callToString(value, env, eval).str)
        }

        builder.append("|]")

        return StringValue(builder.toString())
    }
}

