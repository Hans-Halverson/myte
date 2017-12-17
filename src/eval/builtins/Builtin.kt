package myte.eval.builtins

import myte.eval.*
import myte.eval.values.*
import myte.parser.*
import myte.shared.*

abstract class Builtin(val name: String, val type: FunctionType) {
	private var ident: Identifier? = null

	abstract fun eval(args: List<Value>): Value

	fun evalWrapper(args: List<Value>): Value {
		val actualTypes = args.map { arg -> arg.type }
		if (actualTypes != type.argTypes) {
			throw EvaluationException("${name} expected arguments of type ${type.argTypes}, but found {actualTypes}")
		}

		return eval(args)
	}

	fun register(symbolTable: SymbolTable, environment: Environment): Identifier {
		val ident = symbolTable.addSymbol(name, IdentifierClass.FUNCTION, type)
		val builtin = BuiltinValue(ident, this::evalWrapper, type)
		environment.extend(ident, builtin)

		return ident
	}

	fun getIdent(): Identifier = ident!!

	fun setIdent(id: Identifier) {
		ident = id
	}
}

val BUILTINS: Map<String, Builtin> = hashMapOf(
	INT_TO_FLOAT_BUILTIN to IntToFloatBuiltin()
)

fun registerBuiltins(symbolTable: SymbolTable, environment: Environment) {
	for ((_, builtin) in BUILTINS) {
		builtin.setIdent(builtin.register(symbolTable, environment))
	}
}
