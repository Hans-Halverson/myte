package myte.eval.builtins

import myte.eval.*
import myte.eval.values.*
import myte.parser.*
import myte.shared.*

/**
 * A builtin function for the myte language.
 */
abstract class Builtin(val name: String, val type: FunctionType) {
    private var identIfRegistered: Identifier? = null

    /**
     * Apply this builtin function to a list of values.
     * @param args a list of values to be used as arguments to this builtin
     * @return a value that is the result of applying the builtin function to the
     *         input list of values
     */
    abstract fun eval(args: List<Value>): Value

    /**
     * Apply the builtin function with the implementation of eval, and check that input args
     * have the correct types.
     */
    fun evalWrapper(args: List<Value>): Value {
        val actualTypes = args.map { arg -> arg.type }
        if (actualTypes != type.argTypes) {
            throw Exception("${name} expected arguments of type ${type.argTypes}, but " +
                    "found ${actualTypes}")
        }

        return eval(args)
    }

    /**
     * Add this builtin to the symbol table and environment.
     */
    fun register(symbolTable: SymbolTable, environment: Environment): Identifier {
        // Add type annotated identifier to symbol table
        val ident = symbolTable.addBuiltin(name)
        val info = symbolTable.getInfo(ident)!!
        info.type = type

        val builtin = BuiltinValue(this::evalWrapper, type)
        environment.extendGlobal(ident, builtin)

        return ident
    }

    var ident: Identifier
        get() = identIfRegistered!!
        set(newIdent: Identifier) {
            identIfRegistered = newIdent
        }
}

abstract class BuiltinMethod(val name: String, val type: FunctionType, val receiverType: Type) {
    /**
     * Apply this builtin method to a list of values, with the specified receiver.
     * @param args a list of values to be used as arguments to this builtin
     * @param recv the value that the method is being called on
     * @return a value that is the result of applying the builtin function to the
     *         input list of values
     */
    abstract fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value
}

// Map of all builtin functions
val BUILTINS: Map<String, Builtin> = hashMapOf(
    PRINT_BUILTIN to PrintBuiltin(),
    PRINT_LINE_BUILTIN to PrintLineBuiltin()
)

val BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    BYTE_ADD_METHOD to ByteAddBuiltinMethod(),
    BYTE_SUBTRACT_METHOD to ByteSubtractBuiltinMethod(),
    BYTE_MULTIPLY_METHOD to ByteMultiplyBuiltinMethod(),
    BYTE_DIVIDE_METHOD to ByteDivideBuiltinMethod(),
    BYTE_POWER_METHOD to BytePowerBuiltinMethod(),
    BYTE_REMAINDER_METHOD to ByteRemainderBuiltinMethod(),
    BYTE_UNARY_PLUS_METHOD to ByteUnaryPlusBuiltinMethod(),
    BYTE_UNARY_MINUS_METHOD to ByteUnaryMinusBuiltinMethod(),
    BYTE_TO_INT_METHOD to ByteToIntBuiltinMethod(),
    BYTE_TO_FLOAT_METHOD to ByteToFloatBuiltinMethod(),
    BYTE_TO_DOUBLE_METHOD to ByteToDoubleBuiltinMethod(),
    BYTE_COMPARE_METHOD to ByteCompareBuiltinMethod(),
    BYTE_EQUALS_METHOD to ByteEqualsBuiltinMethod(),
    BYTE_TO_STRING_METHOD to ByteToStringBuiltinMethod(),

    INT_ADD_METHOD to IntAddBuiltinMethod(),
    INT_SUBTRACT_METHOD to IntSubtractBuiltinMethod(),
    INT_MULTIPLY_METHOD to IntMultiplyBuiltinMethod(),
    INT_DIVIDE_METHOD to IntDivideBuiltinMethod(),
    INT_POWER_METHOD to IntPowerBuiltinMethod(),
    INT_REMAINDER_METHOD to IntRemainderBuiltinMethod(),
    INT_UNARY_PLUS_METHOD to IntUnaryPlusBuiltinMethod(),
    INT_UNARY_MINUS_METHOD to IntUnaryMinusBuiltinMethod(),
    INT_TO_BYTE_METHOD to IntToByteBuiltinMethod(),
    INT_TO_FLOAT_METHOD to IntToFloatBuiltinMethod(),
    INT_TO_DOUBLE_METHOD to IntToDoubleBuiltinMethod(),
    INT_COMPARE_METHOD to IntCompareBuiltinMethod(),
    INT_EQUALS_METHOD to IntEqualsBuiltinMethod(),
    INT_TO_STRING_METHOD to IntToStringBuiltinMethod(),

    FLOAT_ADD_METHOD to FloatAddBuiltinMethod(),
    FLOAT_SUBTRACT_METHOD to FloatSubtractBuiltinMethod(),
    FLOAT_MULTIPLY_METHOD to FloatMultiplyBuiltinMethod(),
    FLOAT_DIVIDE_METHOD to FloatDivideBuiltinMethod(),
    FLOAT_POWER_METHOD to FloatPowerBuiltinMethod(),
    FLOAT_REMAINDER_METHOD to FloatRemainderBuiltinMethod(),
    FLOAT_UNARY_PLUS_METHOD to FloatUnaryPlusBuiltinMethod(),
    FLOAT_UNARY_MINUS_METHOD to FloatUnaryMinusBuiltinMethod(),
    FLOAT_TO_BYTE_METHOD to FloatToByteBuiltinMethod(),
    FLOAT_TO_INT_METHOD to FloatToIntBuiltinMethod(),
    FLOAT_TO_DOUBLE_METHOD to FloatToDoubleBuiltinMethod(),
    FLOAT_COMPARE_METHOD to FloatCompareBuiltinMethod(),
    FLOAT_EQUALS_METHOD to FloatEqualsBuiltinMethod(),
    FLOAT_TO_STRING_METHOD to FloatToStringBuiltinMethod(),

    DOUBLE_ADD_METHOD to DoubleAddBuiltinMethod(),
    DOUBLE_SUBTRACT_METHOD to DoubleSubtractBuiltinMethod(),
    DOUBLE_MULTIPLY_METHOD to DoubleMultiplyBuiltinMethod(),
    DOUBLE_DIVIDE_METHOD to DoubleDivideBuiltinMethod(),
    DOUBLE_POWER_METHOD to DoublePowerBuiltinMethod(),
    DOUBLE_REMAINDER_METHOD to DoubleRemainderBuiltinMethod(),
    DOUBLE_UNARY_PLUS_METHOD to DoubleUnaryPlusBuiltinMethod(),
    DOUBLE_UNARY_MINUS_METHOD to DoubleUnaryMinusBuiltinMethod(),
    DOUBLE_TO_BYTE_METHOD to DoubleToByteBuiltinMethod(),
    DOUBLE_TO_INT_METHOD to DoubleToIntBuiltinMethod(),
    DOUBLE_TO_FLOAT_METHOD to DoubleToFloatBuiltinMethod(),
    DOUBLE_COMPARE_METHOD to DoubleCompareBuiltinMethod(),
    DOUBLE_EQUALS_METHOD to DoubleEqualsBuiltinMethod(),
    DOUBLE_TO_STRING_METHOD to DoubleToStringBuiltinMethod(),

    STRING_SIZE_METHOD to StringSizeBuiltinMethod(),
    STRING_ADD_METHOD to StringAddBuiltinMethod(),
    STRING_COMPARE_METHOD to StringCompareBuiltinMethod(),
    STRING_EQUALS_METHOD to StringEqualsBuiltinMethod(),

    VECTOR_INDEX_METHOD to VectorIndexBuiltinMethod(),
    VECTOR_INDEX_ASSIGN_METHOD to VectorIndexAssignBuiltinMethod(),
    VECTOR_ADD_METHOD to VectorAddBuiltinMethod(),
    VECTOR_REMOVE_METHOD to VectorRemoveBuiltinMethod(),
    VECTOR_SIZE_METHOD to VectorSizeBuiltinMethod(),
    VECTOR_TO_STRING_METHOD to VectorToStringBuiltinMethod(),

    SET_ADD_METHOD to SetAddBuiltinMethod(),
    SET_CONTAINS_METHOD to SetContainsBuiltinMethod(),
    SET_REMOVE_METHOD to SetRemoveBuiltinMethod(),
    SET_SIZE_METHOD to SetSizeBuiltinMethod(),
    SET_TO_VEC_METHOD to SetToVecBuiltinMethod(),
    SET_TO_STRING_METHOD to SetToStringBuiltinMethod(),

    MAP_INDEX_METHOD to MapIndexBuiltinMethod(),
    MAP_INDEX_ASSIGN_METHOD to MapIndexAssignBuiltinMethod(),
    MAP_REMOVE_METHOD to MapRemoveBuiltinMethod(),
    MAP_SIZE_METHOD to MapSizeBuiltinMethod(),
    MAP_KEYS_METHOD to MapKeysBuiltinMethod(),
    MAP_VALUES_METHOD to MapValuesBuiltinMethod(),
    MAP_CONTAINS_KEY_METHOD to MapContainsKeyBuiltinMethod(),
    MAP_CONTAINS_VALUE_METHOD to MapContainsValueBuiltinMethod(),
    MAP_TO_VEC_METHOD to MapToVecBuiltinMethod(),
    MAP_TO_STRING_METHOD to MapToStringBuiltinMethod(),

    TUPLE_TO_STRING_METHOD to TupleToStringBuiltinMethod()
)
