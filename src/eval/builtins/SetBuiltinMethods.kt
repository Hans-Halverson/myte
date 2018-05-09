package myte.eval.builtins

import myte.eval.values.*
import myte.shared.*

val BUILTIN_SET_TYPE = SetType(TypeVariable())

const val SET_ADD_METHOD = "add"
const val SET_CONTAINS_METHOD = "contains"
const val SET_REMOVE_METHOD = "remove"
const val SET_SIZE_METHOD = "size"
const val SET_TO_STRING_METHOD = "toString"

val SET_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    SET_ADD_METHOD to SetAddBuiltinMethod(),
    SET_CONTAINS_METHOD to SetContainsBuiltinMethod(),
    SET_REMOVE_METHOD to SetRemoveBuiltinMethod(),
    SET_SIZE_METHOD to SetSizeBuiltinMethod(),
    SET_TO_STRING_METHOD to SetToStringBuiltinMethod()
)

/**
 * A builtin which adds a single item to a set.
 */
class SetAddBuiltinMethod(
) : BuiltinMethod(
    SET_ADD_METHOD,
    FunctionType(listOf(BUILTIN_SET_TYPE.elementType), UnitType),
    BUILTIN_SET_TYPE
) {
    /**
    * Add a single item to a set.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as SetValue
        val newItem = args[0]

        receiver.elements.add(newItem)

        return UnitValue
    }
}

/**
 * A builtin which returns whether a set contains an item.
 */
class SetContainsBuiltinMethod(
) : BuiltinMethod(
    SET_CONTAINS_METHOD,
    FunctionType(listOf(BUILTIN_SET_TYPE.elementType), BoolType),
    BUILTIN_SET_TYPE
) {
    /**
    * Returns whether a set contains an item.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as SetValue
        val item = args[0]

        return BoolValue(receiver.elements.contains(item))
    }
}

/**
 * A builtin which removes an item from a set.
 */
class SetRemoveBuiltinMethod(
) : BuiltinMethod(
    SET_REMOVE_METHOD,
    FunctionType(listOf(BUILTIN_SET_TYPE.elementType), UnitType),
    BUILTIN_SET_TYPE
) {
    /**
    * Remove an item from a set.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as SetValue
        val removeItem = args[0]

        receiver.elements.remove(removeItem)

        return UnitValue
    }
}

/**
 * A builtin which returns the size of a set.
 */
class SetSizeBuiltinMethod(
) : BuiltinMethod(
    SET_SIZE_METHOD,
    FunctionType(listOf(), IntType),
    BUILTIN_SET_TYPE
) {
    /**
    * Return the size of a set.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as SetValue
        return IntValue(receiver.elements.size)
    }
}

/**
 * A builtin which converts a set to a string.
 */
class SetToStringBuiltinMethod(
) : BuiltinMethod(
    SET_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    BUILTIN_SET_TYPE
) {
    /**
    * Converts a set to a string.
    */
    override fun eval(args: List<Value>, recv: Value): Value {
        val receiver = recv as SetValue
        return StringValue(receiver.toString())
    }
}

