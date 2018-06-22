package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val DOUBLE_ADD_METHOD = "double.add"
const val DOUBLE_SUBTRACT_METHOD = "double.subtract"
const val DOUBLE_MULTIPLY_METHOD = "double.multiply"
const val DOUBLE_DIVIDE_METHOD = "double.divide"
const val DOUBLE_POWER_METHOD = "double.power"
const val DOUBLE_REMAINDER_METHOD = "double.remainder"
const val DOUBLE_UNARY_PLUS_METHOD = "double.unaryPlus"
const val DOUBLE_UNARY_MINUS_METHOD = "double.unaryMinus"
const val DOUBLE_TO_BYTE_METHOD = "double.toByte"
const val DOUBLE_TO_INT_METHOD = "double.toInt"
const val DOUBLE_TO_FLOAT_METHOD = "double.toFloat"
const val DOUBLE_COMPARE_METHOD = "double.compare"
const val DOUBLE_EQUALS_METHOD = "double.equals"
const val DOUBLE_TO_STRING_METHOD = "double.toString"

/**
 * A builtin which adds a double to a double.
 */
class DoubleAddBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_ADD_METHOD,
    FunctionType(listOf(DoubleType), DoubleType),
    DoubleType
) {
    /**
    * Adds a double to a double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val otherDouble = args[0] as DoubleValue
        return DoubleValue(receiver.num + otherDouble.num)
    }
}

/**
 * A builtin which subtracts a double from a double.
 */
class DoubleSubtractBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_SUBTRACT_METHOD,
    FunctionType(listOf(DoubleType), DoubleType),
    DoubleType
) {
    /**
    * Subtracts a double from a double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val otherDouble = args[0] as DoubleValue
        return DoubleValue(receiver.num - otherDouble.num)
    }
}

/**
 * A builtin which multiplies a double by another double.
 */
class DoubleMultiplyBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_MULTIPLY_METHOD,
    FunctionType(listOf(DoubleType), DoubleType),
    DoubleType
) {
    /**
    * Multiply a double by another double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val otherDouble = args[0] as DoubleValue
        return DoubleValue(receiver.num * otherDouble.num)
    }
}

/**
 * A builtin which divides a double by another double.
 */
class DoubleDivideBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_DIVIDE_METHOD,
    FunctionType(listOf(DoubleType), DoubleType),
    DoubleType
) {
    /**
    * Divide a double by another double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val otherDouble = args[0] as DoubleValue
        return DoubleValue(receiver.num / otherDouble.num)
    }
}

/**
 * A builtin which calculates a double raised to a double power.
 */
class DoublePowerBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_POWER_METHOD,
    FunctionType(listOf(DoubleType), DoubleType),
    DoubleType
) {
    /**
    * Calculate a double raised to a double power.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val otherDouble = args[0] as DoubleValue
        return DoubleValue(Math.pow(receiver.num, otherDouble.num))
    }
}

/**
 * A builtin which calculates the remainder after dividing a double by another double.
 */
class DoubleRemainderBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_REMAINDER_METHOD,
    FunctionType(listOf(DoubleType), DoubleType),
    DoubleType
) {
    /**
    * Calculate the remainder after dividing a double by another double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val otherDouble= args[0] as DoubleValue
        return DoubleValue(receiver.num % otherDouble.num)
    }
}

/**
 * A builtin which returns the double, unchanged.
 */
class DoubleUnaryPlusBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_UNARY_PLUS_METHOD,
    FunctionType(listOf(), DoubleType),
    DoubleType
) {
    /**
    * Return the double, unchanged.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        return receiver
    }
}

/**
 * A builtin which negates a double.
 */
class DoubleUnaryMinusBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_UNARY_MINUS_METHOD,
    FunctionType(listOf(), DoubleType),
    DoubleType
) {
    /**
    * Negate a double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        return DoubleValue(-receiver.num)
    }
}

/**
 * A builtin which converts a double to a byte.
 */
class DoubleToByteBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_TO_BYTE_METHOD,
    FunctionType(listOf(), ByteType),
    DoubleType
) {
    /**
    * Converts a double to a byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        return ByteValue(receiver.num.toByte())
    }
}


/**
 * A builtin which converts a double to an int.
 */
class DoubleToIntBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_TO_INT_METHOD,
    FunctionType(listOf(), IntType),
    DoubleType
) {
    /**
    * Converts a double to an int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        return IntValue(receiver.num.toInt())
    }
}

/**
 * A builtin which converts a double to a float.
 */
class DoubleToFloatBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_TO_FLOAT_METHOD,
    FunctionType(listOf(), FloatType),
    DoubleType
) {
    /**
    * Converts a double to a float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        return FloatValue(receiver.num.toFloat())
    }
}

/**
 * A builtin which compares two doubles.
 */
class DoubleCompareBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_COMPARE_METHOD,
    FunctionType(listOf(DoubleType), COMPARISON_TYPE),
    DoubleType
) {
    /**
    * Compare two doubles.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val other = args[0] as DoubleValue

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
 * A builtin which determines whether two doubles are equal.
 */
class DoubleEqualsBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_EQUALS_METHOD,
    FunctionType(listOf(DoubleType), BoolType),
    DoubleType
) {
    /**
    * Determine whether two doubles are equal.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        val other = args[0] as DoubleValue
        return BoolValue(receiver.num == other.num)
    }
}

/**
 * A builtin which converts a double to a string.
 */
class DoubleToStringBuiltinMethod(
) : BuiltinMethod(
    DOUBLE_TO_STRING_METHOD,
    TO_STRING_TYPE,
    DoubleType
) {
    /**
    * Converts a double to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as DoubleValue
        return StringValue(receiver.toString())
    }
}
