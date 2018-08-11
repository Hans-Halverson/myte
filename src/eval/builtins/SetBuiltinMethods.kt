package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

val BUILTIN_SET_TYPE = SetType(TypeParameter())

const val SET_ADD_METHOD = "set.add"
const val SET_CONTAINS_METHOD = "set.contains"
const val SET_REMOVE_METHOD = "set.remove"
const val SET_SIZE_METHOD = "set.size"
const val SET_TO_VEC_METHOD = "set.toVec"
const val SET_TO_STRING_METHOD = "set.toString"

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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
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
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as SetValue
        return IntValue(receiver.elements.size)
    }
}

/**
 * A builtin which converts a set into a vec.
 */
class SetToVecBuiltinMethod(
) : BuiltinMethod(
    SET_TO_VEC_METHOD,
    FunctionType(listOf(), VectorType(BUILTIN_SET_TYPE.elementType)),
    BUILTIN_SET_TYPE
) {
    /**
    * Convert a set into a vec.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as SetValue
        val receiverType = recv.type as SetType
        val vectorType = VectorType(receiverType.elementType)

        return VectorValue(receiver.elements.toList().toMutableList(), vectorType)
    }
}

/**
 * A builtin which converts a set to a string.
 */
class SetToStringBuiltinMethod(
) : BuiltinMethod(
    SET_TO_STRING_METHOD,
    TO_STRING_TYPE,
    BUILTIN_SET_TYPE
) {
    /**
    * Converts a set to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as SetValue

        val elementTypes = receiver.elements.map { value ->
            callToString(value, env, eval).str
        }

        return StringValue(elementTypes.joinToString(", ", "{|", "|}"))
    }
}

