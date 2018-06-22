package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val BYTE_ADD_METHOD = "byte.add"
const val BYTE_SUBTRACT_METHOD = "byte.subtract"
const val BYTE_MULTIPLY_METHOD = "byte.multiply"
const val BYTE_DIVIDE_METHOD = "byte.divide"
const val BYTE_POWER_METHOD = "byte.power"
const val BYTE_REMAINDER_METHOD = "byte.remainder"
const val BYTE_UNARY_PLUS_METHOD = "byte.unaryPlus"
const val BYTE_UNARY_MINUS_METHOD = "byte.unaryMinus"
const val BYTE_TO_INT_METHOD = "byte.toInt"
const val BYTE_TO_FLOAT_METHOD = "byte.toFloat"
const val BYTE_TO_DOUBLE_METHOD = "byte.toDouble"
const val BYTE_COMPARE_METHOD = "byte.compare"
const val BYTE_EQUALS_METHOD = "byte.equals"
const val BYTE_TO_STRING_METHOD = "byte.toString"

/**
 * A builtin which adds a byte to a byte.
 */
class ByteAddBuiltinMethod(
) : BuiltinMethod(
    BYTE_ADD_METHOD,
    FunctionType(listOf(ByteType), ByteType),
    ByteType
) {
    /**
    * Adds a byte to a byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val otherByte = args[0] as ByteValue
        return ByteValue((receiver.num + otherByte.num).toByte())
    }
}

/**
 * A builtin which subtracts a byte from a byte.
 */
class ByteSubtractBuiltinMethod(
) : BuiltinMethod(
    BYTE_SUBTRACT_METHOD,
    FunctionType(listOf(ByteType), ByteType),
    ByteType
) {
    /**
    * Subtracts a byte from a byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val otherByte = args[0] as ByteValue
        return ByteValue((receiver.num - otherByte.num).toByte())
    }
}

/**
 * A builtin which multiplies a byte by another byte.
 */
class ByteMultiplyBuiltinMethod(
) : BuiltinMethod(
    BYTE_MULTIPLY_METHOD,
    FunctionType(listOf(ByteType), ByteType),
    ByteType
) {
    /**
    * Multiply a byte by another byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val otherByte = args[0] as ByteValue
        return ByteValue((receiver.num * otherByte.num).toByte())
    }
}

/**
 * A builtin which divides a byte by another byte.
 */
class ByteDivideBuiltinMethod(
) : BuiltinMethod(
    BYTE_DIVIDE_METHOD,
    FunctionType(listOf(ByteType), ByteType),
    ByteType
) {
    /**
    * Divide a byte by another byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val otherByte = args[0] as ByteValue
        return ByteValue((receiver.num / otherByte.num).toByte())
    }
}

/**
 * A builtin which calculates a byte raised to a byte power.
 */
class BytePowerBuiltinMethod(
) : BuiltinMethod(
    BYTE_POWER_METHOD,
    FunctionType(listOf(ByteType), ByteType),
    ByteType
) {
    /**
    * Calculate a byte raised to a byte power.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val otherByte = args[0] as ByteValue
        return ByteValue(Math.pow(receiver.num.toDouble(), otherByte.num.toDouble()).toByte())
    }
}

/**
 * A builtin which calculates the remainder after dividing a byte by another byte.
 */
class ByteRemainderBuiltinMethod(
) : BuiltinMethod(
    BYTE_REMAINDER_METHOD,
    FunctionType(listOf(ByteType), ByteType),
    ByteType
) {
    /**
    * Calculate the remainder after dividing a byte by another byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val otherByte = args[0] as ByteValue
        return ByteValue((receiver.num % otherByte.num).toByte())
    }
}

/**
 * A builtin which returns the byte, unchanged.
 */
class ByteUnaryPlusBuiltinMethod(
) : BuiltinMethod(
    BYTE_UNARY_PLUS_METHOD,
    FunctionType(listOf(), ByteType),
    ByteType
) {
    /**
    * Return the byte, unchanged.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        return receiver
    }
}

/**
 * A builtin which negates a byte.
 */
class ByteUnaryMinusBuiltinMethod(
) : BuiltinMethod(
    BYTE_UNARY_MINUS_METHOD,
    FunctionType(listOf(), ByteType),
    ByteType
) {
    /**
    * Negate a byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        return ByteValue((-receiver.num).toByte())
    }
}

/**
 * A builtin which converts a byte to an int.
 */
class ByteToIntBuiltinMethod(
) : BuiltinMethod(
    BYTE_TO_INT_METHOD,
    FunctionType(listOf(), IntType),
    ByteType
) {
    /**
    * Converts a byte to an int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        return IntValue(receiver.num.toInt())
    }
}

/**
 * A builtin which converts a byte to a float.
 */
class ByteToFloatBuiltinMethod(
) : BuiltinMethod(
    BYTE_TO_FLOAT_METHOD,
    FunctionType(listOf(), FloatType),
    ByteType
) {
    /**
    * Converts a byte to a float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        return FloatValue(receiver.num.toFloat())
    }
}

/**
 * A builtin which converts a byte to a double.
 */
class ByteToDoubleBuiltinMethod(
) : BuiltinMethod(
    BYTE_TO_DOUBLE_METHOD,
    FunctionType(listOf(), DoubleType),
    ByteType
) {
    /**
    * Converts a byte to a double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        return DoubleValue(receiver.num.toDouble())
    }
}

/**
 * A builtin which compares two bytes.
 */
class ByteCompareBuiltinMethod(
) : BuiltinMethod(
    BYTE_COMPARE_METHOD,
    FunctionType(listOf(ByteType), COMPARISON_TYPE),
    ByteType
) {
    /**
    * Compare two bytes.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val other = args[0] as ByteValue

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
 * A builtin which determines whether two bytes are equal.
 */
class ByteEqualsBuiltinMethod(
) : BuiltinMethod(
    BYTE_EQUALS_METHOD,
    FunctionType(listOf(ByteType), BoolType),
    ByteType
) {
    /**
    * Determine whether two bytes are equal.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        val other = args[0] as ByteValue
        return BoolValue(receiver.num == other.num)
    }
}

/**
 * A builtin which converts a byte to a string.
 */
class ByteToStringBuiltinMethod(
) : BuiltinMethod(
    BYTE_TO_STRING_METHOD,
    TO_STRING_TYPE,
    ByteType
) {
    /**
    * Converts a byte to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as ByteValue
        return StringValue(receiver.toString())
    }
}
