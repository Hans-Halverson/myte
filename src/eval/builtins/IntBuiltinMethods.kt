package myte.eval.builtins

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.shared.*

const val INT_ADD_METHOD = "int.add"
const val INT_SUBTRACT_METHOD = "int.subtract"
const val INT_MULTIPLY_METHOD = "int.multiply"
const val INT_DIVIDE_METHOD = "int.divide"
const val INT_POWER_METHOD = "int.power"
const val INT_REMAINDER_METHOD = "int.remainder"
const val INT_UNARY_PLUS_METHOD = "int.unaryPlus"
const val INT_UNARY_MINUS_METHOD = "int.unaryMinus"
const val INT_TO_BYTE_METHOD = "int.toByte"
const val INT_TO_FLOAT_METHOD = "int.toFloat"
const val INT_TO_DOUBLE_METHOD = "int.toDouble"
const val INT_COMPARE_METHOD = "int.compare"
const val INT_EQUALS_METHOD = "int.equals"
const val INT_TO_STRING_METHOD = "int.toString"

/**
 * A builtin which adds an int to an int.
 */
class IntAddBuiltinMethod(
) : BuiltinMethod(
    INT_ADD_METHOD,
    FunctionType(listOf(IntType), IntType),
    IntType
) {
    /**
    * Adds an int to an int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val otherInt = args[0] as IntValue
        return IntValue(receiver.num + otherInt.num)
    }
}

/**
 * A builtin which subtracts an int from an int.
 */
class IntSubtractBuiltinMethod(
) : BuiltinMethod(
    INT_SUBTRACT_METHOD,
    FunctionType(listOf(IntType), IntType),
    IntType
) {
    /**
    * Subtracts an int from an int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val otherInt = args[0] as IntValue
        return IntValue(receiver.num - otherInt.num)
    }
}

/**
 * A builtin which multiplies an int by another int.
 */
class IntMultiplyBuiltinMethod(
) : BuiltinMethod(
    INT_MULTIPLY_METHOD,
    FunctionType(listOf(IntType), IntType),
    IntType
) {
    /**
    * Multiply an int by another int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val otherInt = args[0] as IntValue
        return IntValue(receiver.num * otherInt.num)
    }
}

/**
 * A builtin which divides an int by another int.
 */
class IntDivideBuiltinMethod(
) : BuiltinMethod(
    INT_DIVIDE_METHOD,
    FunctionType(listOf(IntType), IntType),
    IntType
) {
    /**
    * Divide an int by another int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val otherInt = args[0] as IntValue
        return IntValue(receiver.num / otherInt.num)
    }
}

/**
 * A builtin which calculates an int raised to an int power.
 */
class IntPowerBuiltinMethod(
) : BuiltinMethod(
    INT_POWER_METHOD,
    FunctionType(listOf(IntType), IntType),
    IntType
) {
    /**
    * Calculate an int raised to an int power.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val otherInt = args[0] as IntValue
        return IntValue(Math.pow(receiver.num.toDouble(), otherInt.num.toDouble()).toInt())
    }
}

/**
 * A builtin which calculates the remainder after dividing an int by another int.
 */
class IntRemainderBuiltinMethod(
) : BuiltinMethod(
    INT_REMAINDER_METHOD,
    FunctionType(listOf(IntType), IntType),
    IntType
) {
    /**
    * Calculate the remainder after dividing an int by another int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val otherInt = args[0] as IntValue
        return IntValue(receiver.num % otherInt.num)
    }
}

/**
 * A builtin which returns the int, unchanged.
 */
class IntUnaryPlusBuiltinMethod(
) : BuiltinMethod(
    INT_UNARY_PLUS_METHOD,
    FunctionType(listOf(), IntType),
    IntType
) {
    /**
    * Return the int, unchanged.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return receiver
    }
}

/**
 * A builtin which negates an int.
 */
class IntUnaryMinusBuiltinMethod(
) : BuiltinMethod(
    INT_UNARY_MINUS_METHOD,
    FunctionType(listOf(), IntType),
    IntType
) {
    /**
    * Negate an int.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return IntValue(-receiver.num)
    }
}

/**
 * A builtin which converts an int to a byte.
 */
class IntToByteBuiltinMethod(
) : BuiltinMethod(
    INT_TO_BYTE_METHOD,
    FunctionType(listOf(), ByteType),
    IntType
) {
    /**
    * Converts an int to a byte.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return ByteValue(receiver.num.toByte())
    }
}

/**
 * A builtin which converts an int to a float.
 */
class IntToFloatBuiltinMethod(
) : BuiltinMethod(
    INT_TO_FLOAT_METHOD,
    FunctionType(listOf(), FloatType),
    IntType
) {
    /**
    * Converts an int to a float.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return FloatValue(receiver.num.toFloat())
    }
}

/**
 * A builtin which converts an int to a double.
 */
class IntToDoubleBuiltinMethod(
) : BuiltinMethod(
    INT_TO_DOUBLE_METHOD,
    FunctionType(listOf(), DoubleType),
    IntType
) {
    /**
    * Converts an int to a double.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return DoubleValue(receiver.num.toDouble())
    }
}

/**
 * A builtin which compares two ints.
 */
class IntCompareBuiltinMethod(
) : BuiltinMethod(
    INT_COMPARE_METHOD,
    FunctionType(listOf(IntType), COMPARISON_TYPE),
    IntType
) {
    /**
    * Compare two ints.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val other = args[0] as IntValue

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
 * A builtin which determines whether two ints are equal.
 */
class IntEqualsBuiltinMethod(
) : BuiltinMethod(
    INT_EQUALS_METHOD,
    FunctionType(listOf(IntType), BoolType),
    IntType
) {
    /**
    * Determine whether two ints are equal.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        val other = args[0] as IntValue
        return BoolValue(receiver.num == other.num)
    }
}

/**
 * A builtin which converts an int to a string.
 */
class IntToStringBuiltinMethod(
) : BuiltinMethod(
    INT_TO_STRING_METHOD,
    TO_STRING_TYPE,
    IntType
) {
    /**
    * Converts an int to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as IntValue
        return StringValue(receiver.toString())
    }
}
