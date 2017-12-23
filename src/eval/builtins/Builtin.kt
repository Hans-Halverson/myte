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
            throw EvaluationException("${name} expected arguments of type ${type.argTypes}, but " +
                    "found {actualTypes}")
        }

        return eval(args)
    }

    /**
     * Add this builtin to the symbol table and environment.
     */
    fun register(symbolTable: SymbolTable, environment: Environment): Identifier {
        val ident = symbolTable.addSymbol(name, IdentifierClass.FUNCTION, expressionFromType(type))
        val builtin = BuiltinValue(ident, this::evalWrapper, type)
        environment.extend(ident, builtin)

        return ident
    }

    var ident: Identifier
        get() = identIfRegistered!!
        set(newIdent: Identifier) {
            identIfRegistered = newIdent
        }
}

// List of all builtins
val BUILTINS: Map<String, Builtin> = hashMapOf(
    FLOAT_TO_INT_BUILTIN to FloatToIntBuiltin(),
    INT_TO_FLOAT_BUILTIN to IntToFloatBuiltin()
)

/**
 * Add all builtins to the symbol table and environment. This must be done before parsing.
 */
fun registerBuiltins(symbolTable: SymbolTable, environment: Environment) {
    for ((_, builtin) in BUILTINS) {
        builtin.ident = builtin.register(symbolTable, environment)
    }
}
