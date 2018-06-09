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
    INT_TO_FLOAT_METHOD to IntToFloatBuiltinMethod(),
    INT_TO_STRING_METHOD to IntToStringBuiltinMethod(),

    FLOAT_TO_INT_METHOD to FloatToIntBuiltinMethod(),
    FLOAT_TO_STRING_METHOD to FloatToStringBuiltinMethod(),

    STRING_SIZE_METHOD to StringSizeBuiltinMethod(),

    VECTOR_ADD_METHOD to VectorAddBuiltinMethod(),
    VECTOR_REMOVE_METHOD to VectorRemoveBuiltinMethod(),
    VECTOR_SIZE_METHOD to VectorSizeBuiltinMethod(),
    VECTOR_TO_STRING_METHOD to VectorToStringBuiltinMethod(),

    SET_ADD_METHOD to SetAddBuiltinMethod(),
    SET_CONTAINS_METHOD to SetContainsBuiltinMethod(),
    SET_REMOVE_METHOD to SetRemoveBuiltinMethod(),
    SET_SIZE_METHOD to SetSizeBuiltinMethod(),
    SET_TO_STRING_METHOD to SetToStringBuiltinMethod(),

    MAP_REMOVE_METHOD to MapRemoveBuiltinMethod(),
    MAP_SIZE_METHOD to MapSizeBuiltinMethod(),
    MAP_KEYS_METHOD to MapKeysBuiltinMethod(),
    MAP_VALUES_METHOD to MapValuesBuiltinMethod(),
    MAP_CONTAINS_KEY_METHOD to MapContainsKeyBuiltinMethod(),
    MAP_CONTAINS_VALUE_METHOD to MapContainsValueBuiltinMethod(),
    MAP_TO_STRING_METHOD to MapToStringBuiltinMethod(),

    TUPLE_TO_STRING_METHOD to TupleToStringBuiltinMethod()
)
