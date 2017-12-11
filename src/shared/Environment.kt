package myte.shared

import myte.eval.values.*

import java.util.Stack

typealias Scope = MutableMap<Identifier, Value>

class EnvironmentException(message: String) : Exception(message)

class Environment() {
	private val scopes: Stack<Scope> = Stack()

	init {
		scopes.push(hashMapOf())
	}

	fun lookup(ident: Identifier): Value {
		for (i in scopes.size - 1 downTo 0) {
			val scope = scopes.get(i)
			val value = scope[ident]
			if (value != null) {
				return value
			}
		}

		throw EnvironmentException("Nothing bound to identifier ${ident}")
	}

	fun extend(ident: Identifier, value: Value) {
		val scope = scopes.peek()
		scope[ident] = value
	}

	fun reassign(ident: Identifier, value: Value) {
		for (i in scopes.size - 1 downTo 0) {
			val scope = scopes.get(i)
			if (scope[ident] != null) {
				scope[ident] = value
				return
			}
		}

		throw EnvironmentException("Identifer ${ident} has not been declared")
	}

	fun enterScope() {
		scopes.push(hashMapOf())
	}

	fun exitScope() {
		scopes.pop()
	}

	fun copy(): Environment {
		val newEnv = Environment()
		for (scope in scopes) {
			newEnv.scopes.push(scope)
		}

		return newEnv
	}

}
