package myte.shared

import myte.parser.ImportContext
import myte.parser.Package

class SymbolTableException(message: String) : Exception(message)
class SymbolTableExceptionWithLocation(
    message: String,
    location: Location
) : ExceptionWithLocation(message, location)

enum class ScopeType {
    ROOT,
    REPL,
    BLOCK,
    PACKAGE,
    FUNCTION,
    PATTERN,
    TYPE_DEFINITION,
    NEW_DEFINITION
}

enum class WhichScope {
    CURRENT,
    PACKAGE
}

/**
 * A single scope, which may be nested inside arbitrarily many parent scopes.
 *
 * @param parent the parent scope for this scope, or null if this is the package scope
 * @param idents a map of names to identifiers defined within this scope
 * @param types a map of names to type identifiers defined within this scope
 */
class Scope(
    var parent: Scope? = null,
    val type: ScopeType = ScopeType.PACKAGE,
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
        val parentScope = parent
        val ident = variables[name]
        if (ident != null) {
            return ident
        } else if (parentScope == null) {
            return null
        } else {
            return parentScope.lookupVariable(name)
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
        val parentScope = parent
        val ident = types[name]
        if (ident != null) {
            return ident
        } else if (parentScope == null) {
            return null
        } else {
            return parentScope.lookupType(name)
        }
    }

    fun findPackageScope(): Scope {
        // The REPL scope is effectively a package scope
        if (type == ScopeType.PACKAGE || type == ScopeType.REPL) {
            return this
        } else {
            return parent!!.findPackageScope()
        }
    }
}

class SymbolTable() {
    // The root scope which contains all builtins
    var rootScope: Scope = Scope(null, ScopeType.ROOT)

    // The current scope, intially just a package scope
    var currentScope: Scope = Scope(rootScope)

    // A map of identifiers to their info for all identifiers that have been seen in all scopes
    var identifiers: MutableMap<Identifier, IdentifierInfo> = hashMapOf()

    /**
     * Use a new package's scope as the root package scope.
     */
    fun registerPackageScope(pack: Package) {
        pack.scope.parent = rootScope
        currentScope = pack.scope
    }

    /**
     * Enter a new scope.
     */
    fun enterScope(type: ScopeType) {
        currentScope = Scope(currentScope, type)
    }

    /**
     * Exit the current scope.
     *
     * @throws SymbolTableException if one attempts to exit the package scope
     */
    fun exitScope() {
        val parent = currentScope.parent
        if (parent != null) {
            currentScope = parent
        } else {
            throw SymbolTableException("Cannot exit package scope")
        }
    }

    /**
     * Reset the symbol table to the package scope.
     */
    fun returnToPackageScope() {
        currentScope = currentScope.findPackageScope()
    }

    fun lookupVariable(
        name: String,
        scopePrefixes: List<String>,
        location: Location,
        importContext: ImportContext
    ): VariableSymbolPendingResolution {
        return VariableSymbolPendingResolution(name, scopePrefixes, location,
                currentScope, this, importContext)
    }

    fun addBuiltin(name: String): Identifier {
        return addSymbolInScope(rootScope, name, IdentifierClass.FUNCTION,
                NO_LOCATION, hashSetOf(), true)
    }

    fun addMethod(name: String, location: Location): Identifier {
        val ident = Identifier(name)
        val info = IdentifierInfo(name, IdentifierClass.FUNCTION, location, setOf())

        identifiers[ident] = info

        return ident
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
            WhichScope.PACKAGE -> currentScope.findPackageScope()
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
            WhichScope.PACKAGE -> currentScope.findPackageScope()
        }

        return addSymbolInScope(scope, name, idClass, location, props, false)
    }

    fun addPatternVariable(
        name: String,
        scopePrefixes: List<String>,
        importContext: ImportContext,
        location: Location,
        canCreate: Boolean
    ): PatternSymbolPendingResolution {
        return PatternSymbolPendingResolution(name, scopePrefixes, location, canCreate,
                currentScope, this, importContext)
    }

    fun addTypeVariable(
        name: String,
        scopePrefixes: List<String>,
        importContext: ImportContext,
        location: Location,
        create: Boolean
    ): TypeSymbolPendingResolution {
        return TypeSymbolPendingResolution(name, scopePrefixes, location, create, currentScope,
                this, importContext)
    }

    /**
     * Add a new identifier to a particular scope, and fill in its info.
     *
     * @param scope the scope to add the identifier to
     * @param name the name of the new identifier
     * @param idClass the class of the new identifier (e.g. variable, function)
     * @param location the location of the identifier in the source code
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
            // Cannot add two identifiers with same name to the same package scope
            if (scope.type == ScopeType.PACKAGE && scope.variables.contains(name)) {
                throw SymbolTableExceptionWithLocation("Identifier with name ${name} already " +
                        "exists in this package", location)
            }

            scope.variables[name] = ident
        } else {
            // Cannot add two types with same name to the same package scope
            if ((scope.type == ScopeType.PACKAGE || scope.type == ScopeType.REPL) &&
                    scope.types.contains(name)) {
                throw SymbolTableExceptionWithLocation("Type with name ${name} already exists in " +
                        "this package", location)
            }

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
     * the same package scope. The new symbol table is returned in the package scope.
     */
    fun copyForRepl(): SymbolTable {
        // Create a new symbol table
        val symbolTable = SymbolTable()      

        // Create a new map for the package scope, so that it can be modified
        // without affecting this table  
        val packageScope = currentScope.findPackageScope()
        symbolTable.currentScope = Scope(rootScope, ScopeType.REPL,
                packageScope.variables.toMutableMap(), packageScope.types.toMutableMap())

        // Copy all identifiers to a new map so they can be modified without affecting this table
        symbolTable.identifiers = identifiers.toMutableMap()

        // Preserve same root scope in new symbol table
        symbolTable.rootScope = rootScope

        return symbolTable
    }
}
