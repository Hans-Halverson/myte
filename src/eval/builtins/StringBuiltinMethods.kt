package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val STRING_SIZE_METHOD = "string.size"
const val STRING_ADD_METHOD = "string.add"
const val STRING_COMPARE_METHOD = "string.compare"
const val STRING_EQUALS_METHOD = "string.equals"

/**
 * A builtin which returns the length of a string.
 */
class StringSizeBuiltinMethod(
) : BuiltinMethod(
    STRING_SIZE_METHOD,
    FunctionType(listOf(), IntType),
    StringType
) {
    /**
    * Returns the length of a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as StringValue
        return IntValue(receiver.str.length)
    }
}

/**
 * A builtin which concatenates two strings.
 */
class StringAddBuiltinMethod(
) : BuiltinMethod(
    STRING_ADD_METHOD,
    FunctionType(listOf(StringType), StringType),
    StringType
) {
    /**
    * Concatenate two strings.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as StringValue
        val otherStr = args[0] as StringValue
        return StringValue(receiver.str + otherStr.str)
    }
}

/**
 * A builtin which compares two strings.
 */
class StringCompareBuiltinMethod(
) : BuiltinMethod(
    STRING_COMPARE_METHOD,
    FunctionType(listOf(StringType), COMPARISON_TYPE),
    StringType
) {
    /**
    * Compare two strings.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as StringValue
        val other = args[0] as StringValue

        val variant = if (receiver.str < other.str) {
            getVariantForBuiltinType(COMPARISON_TYPE_SIG, COMPARISON_TYPE_LESS_VARIANT)
        } else if (receiver.str > other.str) {
            getVariantForBuiltinType(COMPARISON_TYPE_SIG, COMPARISON_TYPE_GREATER_VARIANT)
        } else {
            getVariantForBuiltinType(COMPARISON_TYPE_SIG, COMPARISON_TYPE_EQUAL_VARIANT)
        }

        return TupleVariantValue(variant, listOf(), COMPARISON_TYPE)
    }
}

/**
 * A builtin which determines whether two strings are equal.
 */
class StringEqualsBuiltinMethod(
) : BuiltinMethod(
    STRING_EQUALS_METHOD,
    FunctionType(listOf(StringType), BoolType),
    StringType
) {
    /**
    * Determine whether two strings are equal.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as StringValue
        val other = args[0] as StringValue
        return BoolValue(receiver.str == other.str)
    }
}
