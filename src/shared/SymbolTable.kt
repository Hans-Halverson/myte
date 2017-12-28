package myte.shared

import java.util.Stack

class SymbolTableException(message: String) : Exception(message)

class SymbolTable() {
    // A stack of scopes, where each scope is a map of strings to identifiers
    private val scopes: Stack<MutableMap<String, Identifier>> = Stack()

    // A map of identifiers to their info for all identifiers that have been seen in all scopes
    var identifiers: MutableMap<Identifier, IdentifierInfo> = hashMapOf()

    // Start off with a single global scope
    init {
        scopes.push(hashMapOf())
    }

    /**
     * Enter a new scope.
     */
    fun enterScope() {
        scopes.push(hashMapOf())
    }

    /**
     * Exit the current scope.
     *
     * @throws SymbolTableException if one attempts to exit the global scope
     */
    fun exitScope() {
        if (scopes.size > 1) {
            scopes.pop()
        } else {
            throw SymbolTableException("Cannot exit global scope")
        }
    }

    /**
     * Reset the symbol table to the global scope.
     */
    fun returnToGlobalScope() {
        while (scopes.size > 1) {
            scopes.pop()
        }
    }

    /**
     * Lookup a string in the current scope.
     *
     * @return the identifier for the given string in the current scope, or null if no identifier
     *         with that name has been seen in the current scope
     */
    fun lookup(name: String): Identifier? {
        // Find the lowest scope in which this name appears
        for (i in scopes.size - 1 downTo 0) {
            val scope = scopes.get(i)
            val ident = scope[name]
            if (ident != null) {
                return ident
            }
        }

        return null
    }

    fun addSymbol(
        name: String,
        idClass: IdentifierClass,
        type: Type,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        val scope = scopes.peek()
        return addSymbolInScope(scope, name, idClass, type, props)
    }

    fun addSymbolInPreviousScope(
        name: String,
        idClass: IdentifierClass,
        type: Type,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        val scope = scopes.get(scopes.size - 2)
        return addSymbolInScope(scope, name, idClass, type, props)
    }

    fun addSymbolInGlobalScope(
        name: String,
        idClass: IdentifierClass,
        type: Type,
        props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        val scope = scopes[0]
        return addSymbolInScope(scope, name, idClass, type, props)
    }

    /**
     * Add a new identifier to a particular scope, and fill in its info.
     *
     * @param scope the scope to add the identifier to
     * @param name the name of the new identifier
     * @param idClass the class of the new identifier (e.g. variable, function)
     * @param type the best known type for the new identifier
     * @param props an (optional) set of all properties of this identifier
     */
    private fun addSymbolInScope(
        scope: MutableMap<String, Identifier>,
        name: String,
        idClass: IdentifierClass,
        type: Type,
        props: Set<IdentifierProperty>
    ): Identifier {
        val ident = Identifier(name)
        val info = IdentifierInfo(name, idClass, type, props)

        scope[name] = ident
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
     * Return a shallow copy of this symbol table that points to the same data, but in which new
     * symbols can be added without affecting the old symbol table.
     */
    fun copy(): SymbolTable {
        // Create a new symbol table and remove the empty global scope
        val symbolTable = SymbolTable()
        symbolTable.scopes.pop()

        // Create a new map for each scope, so that it can be modified without affecting this table
        for (scope in scopes) {
            symbolTable.scopes.push(scope.toMutableMap())
        }

        // Copy all identifiers to a new map so they can be modified without affecting this table
        symbolTable.identifiers = identifiers.toMutableMap()

        return symbolTable
    }
}
