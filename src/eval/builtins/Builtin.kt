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

/**
 * Add all builtins to the symbol table and environment. This must be done before parsing.
 */
fun registerBuiltins(symbolTable: SymbolTable, environment: Environment) {
    for ((_, builtin) in BUILTINS) {
        builtin.ident = builtin.register(symbolTable, environment)
    }
}
