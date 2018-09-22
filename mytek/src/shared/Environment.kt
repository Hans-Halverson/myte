package myte.shared

import myte.eval.values.*

import java.util.Stack

class EnvironmentException(message: String) : Exception(message)

class Environment() {
    private val scopes: Stack<MutableMap<Identifier, Value>> = Stack()

    // Start off with a single global scope
    init {
        scopes.push(hashMapOf())
    }

    /**
     * Return the value bound to an identifier in the environment.
     *
     * @throws EnvironmentException if no value is bound to the given identifier
     */
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

    /**
     * Extend the environment with the given value bound to the given identifier, overwriting
     * any bindings of the current identifier in the current scope, and shadowing any bindings
     * of the current identifier in all parent scopes.
     */
    fun extend(ident: Identifier, value: Value) {
        val scope = scopes.peek()
        scope[ident] = value
    }

    fun extendGlobal(ident: Identifier, value: Value) {
        val scope = scopes[0]
        scope[ident] = value
    }

    /**
     * Reassign the value bound to an identifier in the environment to a new value. This will
     * reassign the value in the lowest scope in which the identifier is bound.
     *
     * @throws EnvironmentException if no value is bound to the given identifier
     */
    fun reassign(ident: Identifier, value: Value) {
        // Find the lowest scope in which this identifier is bound, and reassign it to new value
        for (i in scopes.size - 1 downTo 0) {
            val scope = scopes.get(i)
            if (scope[ident] != null) {
                scope[ident] = value
                return
            }
        }

        throw EnvironmentException("Identifer ${ident} has not been declared")
    }

    /**
     * Enter a new, empty scope.
     */
    fun enterScope() {
        scopes.push(hashMapOf())
    }

    /**
     * Exit the current scope.
     */
    fun exitScope() {
        scopes.pop()
    }

    /**
     * Return a shallow copy of the current environment.
     */
    fun copy(): Environment {
        // Create a new environment and remove the empty global scope
        val newEnv = Environment()
        newEnv.exitScope()

        for (scope in scopes) {
            newEnv.scopes.push(scope)
        }

        return newEnv
    }

}
