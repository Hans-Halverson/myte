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

class VariableSymbolPendingResolution(val name: String, val scope: Scope) : ResolvableSymbol() {
    override fun resolve(): Identifier? {
        var currentScope: Scope? = scope

        while (currentScope != null) {
            // Walk up parent scopes, but can only look up in 1. Global scope since it is unordered,
            // or 2. Pattern scope since pattern variables are always unresolved on first pass
            if (currentScope.type == ScopeType.GLOBAL || currentScope.type == ScopeType.PATTERN) {
                val ident = currentScope.lookupVariable(name) 
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
        // Can only resolve to type constructor or new identifier, and at resolution all ADTs
        // will be defined in the global scope. If no ident exists, create a new variable.
        val ident = scope.lookupVariable(name)
        if (ident == null) {
            val patternIdent = symbolTable.addSymbolInScope(scope, name, IdentifierClass.VARIABLE,
                    location, hashSetOf(), true)

            // Annotate this new identifier with a new type variable
            symbolTable.getInfo(patternIdent)?.type = TypeVariable()

            return patternIdent
        }

        // If the closest ident is not a type constructor, then create a new variable
        val existingIdClass = symbolTable.getInfo(ident)?.idClass
        if (existingIdClass != IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT) {
            val patternIdent = symbolTable.addSymbolInScope(scope, name, IdentifierClass.VARIABLE,
                    location, hashSetOf(), true)

            // Annotate this new identifier with a new type variable
            symbolTable.getInfo(patternIdent)?.type = TypeVariable()

            return patternIdent
        }

        return ident
    }

    override fun symbol(): String = name
}

class TypeSymbolPendingResolution(
    val name: String,
    val location: Location,
    val create: Boolean,
    val scope: Scope,
    val symbolTable: SymbolTable
) : ResolvableSymbol() {
    override fun resolve(): Identifier? {
        var currentScope: Scope? = scope

        while (currentScope != null) {
            // Walk up parent scopes, but can only look up in 1. Global scope since it is unordered
            // and contains type defs, 2. Function scope since type variables are added there,
            // or 3. Type definition scope since type parameters are introduced there
            if (currentScope.type == ScopeType.GLOBAL || currentScope.type == ScopeType.FUNCTION ||
                    currentScope.type == ScopeType.TYPE_DEFINITION) {
                val ident = currentScope.lookupType(name) 
                if (ident != null) {
                    return ident
                }
            }

            currentScope = currentScope.parent
        }

        // If no ident was found, then create a type parameter if create flag is set
        if (create) {
            val typeParamIdent = symbolTable.addSymbolInScope(scope, name,
                    IdentifierClass.TYPE_PARAMETER, location, hashSetOf(), false)

            // Annotate type paramater ident with new type variable
            symbolTable.getInfo(typeParamIdent)?.type = TypeVariable()

            return typeParamIdent
        } else {
            return null
        }
    }

    override fun symbol(): String = name
}

enum class ScopeType {
    BLOCK,
    GLOBAL,
    FUNCTION,
    PATTERN,
    TYPE_DEFINITION
}

enum class WhichScope {
    CURRENT,
    PREVIOUS,
    GLOBAL
}

/**
 * A single scope, which may be nested inside arbitrarily many parent scopes.
 *
 * @param parent the parent scope for this scope, or null if this is the global scope
 * @param idents a map of names to identifiers defined within this scope
 * @param types a map of names to type identifiers defined within this scope
 */
class Scope(
    val parent: Scope? = null,
    val type: ScopeType = ScopeType.GLOBAL,
    val variables: MutableMap<String, Identifier> = mutableMapOf(),
    val types: MutableMap<String, Identifier> = mutableMapOf()
) {
    /**
     * Lookup a variable in the current scope, and in all parent scopes if it is not defined in
     * the current scope.
     *
     * @return the identifier for the given string in the current scope, or null if no variable
     *         with that name has been seen in the current scope
     */
    fun lookupVariable(name: String): Identifier? {
        val ident = variables[name]
        if (ident != null) {
            return ident
        } else if (parent == null) {
            return null
        } else {
            return parent.lookupVariable(name)
        }
    }

    /**
     * Lookup a type identifier in the current scope, and in all parent scopes if it is not defined
     * in the current scope.
     *
     * @return the type identifier for the given string in the current scope, or null if no type
     *         identifier with that name has been seen in the current scope
     */
    fun lookupType(name: String): Identifier? {
        val ident = types[name]
        if (ident != null) {
            return ident
        } else if (parent == null) {
            return null
        } else {
            return parent.lookupType(name)
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
    fun enterScope(type: ScopeType) {
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

    fun lookupVariable(name: String): ResolvableSymbol {
        val ident = currentScope.lookupVariable(name)
        if (ident != null) {
            return ResolvedIdentifier(ident)
        } else {
            return VariableSymbolPendingResolution(name, currentScope)
        }
    }

    fun addVariable(
        name: String,
        idClass: IdentifierClass,
        location: Location,
        whichScope: WhichScope = WhichScope.CURRENT,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        val scope = when (whichScope) {
            WhichScope.CURRENT -> currentScope
            WhichScope.PREVIOUS -> currentScope.parent!!
            WhichScope.GLOBAL -> currentScope.findGlobalScope()
        }

        return addSymbolInScope(scope, name, idClass, location, props, true)
    }

    fun addType(
        name: String,
        idClass: IdentifierClass,
        location: Location,
        whichScope: WhichScope = WhichScope.CURRENT,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        val scope = when (whichScope) {
            WhichScope.CURRENT -> currentScope
            WhichScope.PREVIOUS -> currentScope.parent!!
            WhichScope.GLOBAL -> currentScope.findGlobalScope()
        }

        return addSymbolInScope(scope, name, idClass, location, props, false)
    }

    fun addPatternVariable(name: String, location: Location): PatternSymbolPendingResolution {
        return PatternSymbolPendingResolution(name, location, currentScope, this)
    }

    fun addTypeVariable(
        name: String,
        location: Location,
        create: Boolean
    ): TypeSymbolPendingResolution {
        return TypeSymbolPendingResolution(name, location, create, currentScope, this)
    }

    /**
     * Add a new identifier to a particular scope, and fill in its info.
     *
     * @param scope the scope to add the identifier to
     * @param name the name of the new identifier
     * @param idClass the class of the new identifier (e.g. variable, function)
     * @property location the location of the identifier in the source code
     * @param props the set of all properties of this identifier
     * @param isVariable whether to add this symbol as variable, or whether to add as a type
     */
    fun addSymbolInScope(
        scope: Scope,
        name: String,
        idClass: IdentifierClass,
        location: Location,
        props: Set<IdentifierProperty>,
        isVariable: Boolean
    ): Identifier {
        val ident = Identifier(name)
        val info = IdentifierInfo(name, idClass, location, props)

        if (isVariable) {
            scope.variables[name] = ident
        } else {
            scope.types[name] = ident
        }

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
        val globalScope = currentScope.findGlobalScope()
        symbolTable.currentScope = Scope(null, ScopeType.GLOBAL,
                globalScope.variables.toMutableMap(), globalScope.types.toMutableMap())

        // Copy all identifiers to a new map so they can be modified without affecting this table
        symbolTable.identifiers = identifiers.toMutableMap()

        return symbolTable
    }
}
