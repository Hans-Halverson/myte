package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val FLOAT_ADD_METHOD = "float.add"
const val FLOAT_SUBTRACT_METHOD = "float.subtract"
const val FLOAT_MULTIPLY_METHOD = "float.multiply"
const val FLOAT_DIVIDE_METHOD = "float.divide"
const val FLOAT_POWER_METHOD = "float.power"
const val FLOAT_REMAINDER_METHOD = "float.remainder"
const val FLOAT_UNARY_PLUS_METHOD = "float.unaryPlus"
const val FLOAT_UNARY_MINUS_METHOD = "float.unaryMinus"
const val FLOAT_TO_BYTE_METHOD = "float.toByte"
const val FLOAT_TO_INT_METHOD = "float.toInt"
const val FLOAT_TO_DOUBLE_METHOD = "float.toDouble"
const val FLOAT_COMPARE_METHOD = "float.compare"
const val FLOAT_EQUALS_METHOD = "float.equals"
const val FLOAT_TO_STRING_METHOD = "float.toString"

/**
 * A builtin which adds a float to a float.
 */
class FloatAddBuiltinMethod(
) : BuiltinMethod(
    FLOAT_ADD_METHOD,
    FunctionType(listOf(FloatType), FloatType),
    FloatType
) {
    /**
    * Adds a float to a float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val otherFloat = args[0] as FloatValue
        return FloatValue(receiver.num + otherFloat.num)
    }
}

/**
 * A builtin which subtracts a float from a float.
 */
class FloatSubtractBuiltinMethod(
) : BuiltinMethod(
    FLOAT_SUBTRACT_METHOD,
    FunctionType(listOf(FloatType), FloatType),
    FloatType
) {
    /**
    * Subtracts a float from a float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val otherFloat = args[0] as FloatValue
        return FloatValue(receiver.num - otherFloat.num)
    }
}

/**
 * A builtin which multiplies a float by another float.
 */
class FloatMultiplyBuiltinMethod(
) : BuiltinMethod(
    FLOAT_MULTIPLY_METHOD,
    FunctionType(listOf(FloatType), FloatType),
    FloatType
) {
    /**
    * Multiply a float by another float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val otherFloat = args[0] as FloatValue
        return FloatValue(receiver.num * otherFloat.num)
    }
}

/**
 * A builtin which divides a float by another float.
 */
class FloatDivideBuiltinMethod(
) : BuiltinMethod(
    FLOAT_DIVIDE_METHOD,
    FunctionType(listOf(FloatType), FloatType),
    FloatType
) {
    /**
    * Divide a float by another float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val otherFloat = args[0] as FloatValue
        return FloatValue(receiver.num / otherFloat.num)
    }
}

/**
 * A builtin which calculates a float raised to a float power.
 */
class FloatPowerBuiltinMethod(
) : BuiltinMethod(
    FLOAT_POWER_METHOD,
    FunctionType(listOf(FloatType), FloatType),
    FloatType
) {
    /**
    * Calculate a float raised to a float power.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val otherFloat = args[0] as FloatValue
        return FloatValue(Math.pow(receiver.num.toDouble(), otherFloat.num.toDouble()).toFloat())
    }
}

/**
 * A builtin which calculates the remainder after dividing a float by another float.
 */
class FloatRemainderBuiltinMethod(
) : BuiltinMethod(
    FLOAT_REMAINDER_METHOD,
    FunctionType(listOf(FloatType), FloatType),
    FloatType
) {
    /**
    * Calculate the remainder after dividing a float by another float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val otherFloat= args[0] as FloatValue
        return FloatValue(receiver.num % otherFloat.num)
    }
}

/**
 * A builtin which returns the float, unchanged.
 */
class FloatUnaryPlusBuiltinMethod(
) : BuiltinMethod(
    FLOAT_UNARY_PLUS_METHOD,
    FunctionType(listOf(), FloatType),
    FloatType
) {
    /**
    * Return the float, unchanged.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return receiver
    }
}

/**
 * A builtin which negates a float.
 */
class FloatUnaryMinusBuiltinMethod(
) : BuiltinMethod(
    FLOAT_UNARY_MINUS_METHOD,
    FunctionType(listOf(), FloatType),
    FloatType
) {
    /**
    * Negate a float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return FloatValue(-receiver.num)
    }
}

/**
 * A builtin which converts a float to a byte.
 */
class FloatToByteBuiltinMethod(
) : BuiltinMethod(
    FLOAT_TO_BYTE_METHOD,
    FunctionType(listOf(), ByteType),
    FloatType
) {
    /**
    * Converts a float to a byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return ByteValue(receiver.num.toByte())
    }
}

/**
 * A builtin which converts a float to an int.
 */
class FloatToIntBuiltinMethod(
) : BuiltinMethod(
    FLOAT_TO_INT_METHOD,
    FunctionType(listOf(), IntType),
    FloatType
) {
    /**
    * Converts a float to an int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return IntValue(receiver.num.toInt())
    }
}

/**
 * A builtin which converts a float to a double.
 */
class FloatToDoubleBuiltinMethod(
) : BuiltinMethod(
    FLOAT_TO_DOUBLE_METHOD,
    FunctionType(listOf(), DoubleType),
    FloatType
) {
    /**
    * Converts a float to a double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return DoubleValue(receiver.num.toDouble())
    }
}

/**
 * A builtin which compares two floats.
 */
class FloatCompareBuiltinMethod(
) : BuiltinMethod(
    FLOAT_COMPARE_METHOD,
    FunctionType(listOf(FloatType), COMPARISON_TYPE),
    FloatType
) {
    /**
    * Compare two floats.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val other = args[0] as FloatValue

        val variant = if (receiver.num < other.num) {
            getVariantForBuiltinType(COMPARISON_TYPE_SIG, COMPARISON_TYPE_LESS_VARIANT)
        } else if (receiver.num > other.num) {
            getVariantForBuiltinType(COMPARISON_TYPE_SIG, COMPARISON_TYPE_GREATER_VARIANT)
        } else {
            getVariantForBuiltinType(COMPARISON_TYPE_SIG, COMPARISON_TYPE_EQUAL_VARIANT)
        }

        return TupleVariantValue(variant, listOf(), COMPARISON_TYPE)
    }
}

/**
 * A builtin which determines whether two floats are equal.
 */
class FloatEqualsBuiltinMethod(
) : BuiltinMethod(
    FLOAT_EQUALS_METHOD,
    FunctionType(listOf(FloatType), BoolType),
    FloatType
) {
    /**
    * Determine whether two floats are equal.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        val other = args[0] as FloatValue
        return BoolValue(receiver.num == other.num)
    }
}

/**
 * A builtin which converts a float to a string.
 */
class FloatToStringBuiltinMethod(
) : BuiltinMethod(
    FLOAT_TO_STRING_METHOD,
    TO_STRING_TYPE,
    FloatType
) {
    /**
    * Converts a float to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as FloatValue
        return StringValue(receiver.toString())
    }
}
