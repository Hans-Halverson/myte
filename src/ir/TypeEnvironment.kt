package myte.ir

import myte.shared.*

enum class TypeScopeContext {
    TYPE_DEFINITION,
    TRAIT_DEFINITION,
    TYPE_IMPLEMENTATION,
    FUNCTION_DEFINITION,
    GLOBAL
}

/**
 * A single type scope, which may be nested inside arbitrarily many parent type scopes.
 *
 * @param parent the parent type scope for this type scope, or null if this is the global type scope
 * @param context the context for this type scope
 * @param boundTypeVars a set of all type variables bound in this type scope
 */
class TypeScope(
    val parent: TypeScope?,
    val context: TypeScopeContext,
    val boundTypeVars: MutableSet<TypeVariable> = mutableSetOf()
) {
    /**
     * Return whether the given type variable is bound in this type scope or any parent type scopes.
     */
    fun isTypeVarBound(typeVar: TypeVariable): Boolean {
        if (boundTypeVars.contains(typeVar)) {
            return true
        } else if (parent == null) {
            return false
        } else {
            return parent.isTypeVarBound(typeVar)
        }
    }

    fun findGlobalScope(): TypeScope {
        return parent?.findGlobalScope() ?: this
    }
}

class TypeEnvironment() {
    // The current scope, intially the global scope
    var currentScope: TypeScope = TypeScope(null, TypeScopeContext.GLOBAL)

    /**
     * Enter a new type scope.
     */
    fun enterScope(type: TypeScopeContext) {
        currentScope = TypeScope(currentScope, type)
    }

    /**
     * Exit the current type scope.
     *
     * @throws ExceptionWithoutLocation if one attempts to exit the global type scope
     */
    fun exitScope() {
        val parent = currentScope.parent
        if (parent != null) {
            currentScope = parent
        } else {
            throw ExceptionWithoutLocation("Cannot exit package scope")
        }
    }

    /**
     * Reset the symbol table to the package scope.
     */
    fun returnToGlobalScope() {
        currentScope = currentScope.findGlobalScope()
    }

    fun bindTypeVar(typeVar: TypeVariable) = currentScope.boundTypeVars.add(typeVar)

    fun isTypeVarBound(typeVar: TypeVariable) = currentScope.isTypeVarBound(typeVar)
}
