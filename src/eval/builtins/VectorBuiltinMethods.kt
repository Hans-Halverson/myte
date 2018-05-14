package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

val BUILTIN_VECTOR_TYPE = VectorType(TypeVariable())

const val VECTOR_ADD_METHOD = "add"
const val VECTOR_REMOVE_METHOD = "remove"
const val VECTOR_SIZE_METHOD = "size"

val VECTOR_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    VECTOR_ADD_METHOD to VectorAddBuiltinMethod(),
    VECTOR_REMOVE_METHOD to VectorRemoveBuiltinMethod(),
    VECTOR_SIZE_METHOD to VectorSizeBuiltinMethod(),
    TO_STRING_METHOD to VectorToStringBuiltinMethod()
)

/**
 * A builtin which adds a single item to the end of a vector.
 */
class VectorAddBuiltinMethod(
) : BuiltinMethod(
    VECTOR_ADD_METHOD,
    FunctionType(listOf(BUILTIN_VECTOR_TYPE.elementType), UnitType),
    BUILTIN_VECTOR_TYPE
) {
    /**
    * Add a single item to the end of a vector.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as VectorValue
        val newItem = args[0]

        receiver.elements.add(newItem)

        return UnitValue
    }
}

/**
 * A builtin which removes an item from the specified index of a vector.
 */
class VectorRemoveBuiltinMethod(
) : BuiltinMethod(
    VECTOR_REMOVE_METHOD,
    FunctionType(listOf(IntType), UnitType),
    BUILTIN_VECTOR_TYPE
) {
    /**
    * Remove an item from the specified index of a vector.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as VectorValue
        val removeIndex = args[0] as IntValue

        receiver.elements.removeAt(removeIndex.num)

        return UnitValue
    }
}

/**
 * A builtin which returns the size of a vector.
 */
class VectorSizeBuiltinMethod(
) : BuiltinMethod(
    VECTOR_SIZE_METHOD,
    FunctionType(listOf(), IntType),
    BUILTIN_VECTOR_TYPE
) {
    /**
    * Return the size of a vector.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as VectorValue
        return IntValue(receiver.elements.size)
    }
}

/**
 * A builtin which converts a vector to a string.
 */
class VectorToStringBuiltinMethod(
) : BuiltinMethod(
    TO_STRING_METHOD,
    TO_STRING_TYPE,
    BUILTIN_VECTOR_TYPE
) {
    /**
    * Converts a vector to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as VectorValue

        val elementTypes = receiver.elements.map { value ->
            callToString(value, env, eval).str
        }

        return StringValue(elementTypes.joinToString(", ", "[", "]"))
    }
}
