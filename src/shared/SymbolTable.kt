package myte.shared

class SymbolTableException(message: String) : Exception(message)


abstract class ResolvableSymbol() {
    /**
     * Resolve this symbol into an identifier, or return null if it could not be resolved.
     */
    abstract fun resolve(): Identifier?

    abstract fun symbol(): String
}

class ResolvedIdentifier(val ident: Identifier) : ResolvableSymbol() {
    override fun resolve(): Identifier? = ident
    override fun symbol(): String = ident.name
}

class SymbolPendingResolution(val name: String, val scope: Scope) : ResolvableSymbol() {
    override fun resolve(): Identifier? {
        var currentScope: Scope? = scope

        while (currentScope != null) {
            // Walk up parent scopes, but can only look up in 1. Global scope since it is unordered,
            // or 2. Pattern scope since pattern variables are always unresolved on first pass
            if (currentScope.type == ScopeType.GLOBAL || currentScope.type == ScopeType.PATTERN) {
                val ident = currentScope.lookup(name) 
                if (ident != null) {
                    return ident
                }
            }

            currentScope = currentScope.parent
        }

        return null
    }

    override fun symbol(): String = name
}

class PatternSymbolPendingResolution(
    val name: String,
    val location: Location,
    val scope: Scope,
    val symbolTable: SymbolTable
) : ResolvableSymbol() {
    override fun resolve(): Identifier? {
        val ident = scope.lookup(name)
        if (ident == null) {
            return symbolTable.addSymbolInScope(scope, name, IdentifierClass.VARIABLE, location,
                    TypeVariable(), hashSetOf())
        }

        val existingClass = symbolTable.getInfo(ident)?.idClass
        if (existingClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            return symbolTable.addSymbolInScope(scope, name, IdentifierClass.VARIABLE, location,
                    TypeVariable(), hashSetOf())
        }

        return ident
    }

    override fun symbol(): String = name
}

enum class ScopeType {
    REGULAR,
    GLOBAL,
    PATTERN
}

/**
 * A single scope, which may be nested inside arbitrarily many parent scopes.
 *
 * @param parent the parent scope for this scope, or null if this is the global scope
 * @param idents a map of names to identifiers defined within this scope
 */
class Scope(
    val parent: Scope? = null,
    val type: ScopeType = ScopeType.GLOBAL,
    val idents: MutableMap<String, Identifier> = mutableMapOf()
) {
    /**
     * Lookup a string in the current scope, and in all parent scopes if it is not defined in the
     * current scope.
     *
     * @return the identifier for the given string in the current scope, or null if no identifier
     *         with that name has been seen in the current scope
     */
    fun lookup(name: String): Identifier? {
        val ident = idents[name]
        if (ident != null) {
            return ident
        } else if (parent == null) {
            return null
        } else {
            return parent.lookup(name)
        }
    }

    fun findGlobalScope(): Scope {
        if (parent == null) {
            return this
        } else {
            return parent.findGlobalScope()
        }
    }
}

class SymbolTable() {
    // The current scope, intially just the global scope
    var currentScope: Scope = Scope()

    // A map of identifiers to their info for all identifiers that have been seen in all scopes
    var identifiers: MutableMap<Identifier, IdentifierInfo> = hashMapOf()

    /**
     * Enter a new scope.
     */
    fun enterScope(type: ScopeType = ScopeType.REGULAR) {
        currentScope = Scope(currentScope, type)
    }

    /**
     * Exit the current scope.
     *
     * @throws SymbolTableException if one attempts to exit the global scope
     */
    fun exitScope() {
        val parent = currentScope.parent
        if (parent != null) {
            currentScope = parent
        } else {
            throw SymbolTableException("Cannot exit global scope")
        }
    }

    /**
     * Reset the symbol table to the global scope.
     */
    fun returnToGlobalScope() {
        currentScope = currentScope.findGlobalScope()
    }

    fun lookupDEPRECATED(name: String): Identifier? = currentScope.lookup(name)

    fun lookup(name: String): ResolvableSymbol {
        val ident = currentScope.lookup(name)
        if (ident != null) {
            return ResolvedIdentifier(ident)
        } else {
            return SymbolPendingResolution(name, currentScope)
        }
    }

    fun addSymbol(
        name: String,
        idClass: IdentifierClass,
        location: Location,
        type: Type,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        return addSymbolInScope(currentScope, name, idClass, location, type, props)
    }

    fun addSymbolInPreviousScope(
        name: String,
        idClass: IdentifierClass,
        location: Location,
        type: Type,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        return addSymbolInScope(currentScope.parent!!, name, idClass, location, type, props)
    }

    fun addSymbolInGlobalScope(
        name: String,
        idClass: IdentifierClass,
        location: Location,
        type: Type,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        return addSymbolInScope(currentScope.findGlobalScope(), name, idClass, location,
                type, props)
    }

    fun addPatternSymbol(name: String, location: Location): PatternSymbolPendingResolution {
        return PatternSymbolPendingResolution(name, location, currentScope, this)
    }

    /**
     * Add a new identifier to a particular scope, and fill in its info.
     *
     * @param scope the scope to add the identifier to
     * @param name the name of the new identifier
     * @param idClass the class of the new identifier (e.g. variable, function)
     * @property location the location of the identifier in the source code
     * @param type the best known type for the new identifier
     * @param props an (optional) set of all properties of this identifier
     */
    fun addSymbolInScope(
        scope: Scope,
        name: String,
        idClass: IdentifierClass,
        location: Location,
        type: Type,
        props: Set<IdentifierProperty>
    ): Identifier {
        val ident = Identifier(name)
        val info = IdentifierInfo(name, idClass, location, type, props)

        scope.idents[name] = ident
        identifiers[ident] = info

        return ident
    }

    /**
     * Return the info for any identifer that has been seen so far (if it exists),
     * regardless of scope.
     */
    fun getInfo(ident: Identifier): IdentifierInfo? {
        return identifiers[ident]
    }

    /**
     * Return a shallow copy of this symbol table that has the same set of saved identifiers and
     * the same global scope. The new symbol table is returned in the global scope.
     */
    fun copy(): SymbolTable {
        // Create a new symbol table and remove the empty global scope
        val symbolTable = SymbolTable()      

        // Create a new map for the global scope, so that it can be modified
        // without affecting this table  
        symbolTable.currentScope = Scope(null, ScopeType.GLOBAL,
                currentScope.findGlobalScope().idents.toMutableMap())

        // Copy all identifiers to a new map so they can be modified without affecting this table
        symbolTable.identifiers = identifiers.toMutableMap()

        return symbolTable
    }
}
