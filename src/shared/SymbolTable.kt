package myte.shared

class SymbolTableException(message: String) : Exception(message)

/**
 * A symbol table for a single scope.
 *
 * @property parent the (optional) parent symbol table for this symbol table. If there is no
 *           parent symbol table, this is the global symbol table.
 * @property symbols a map of strings to identifiers in the current scope
 */
private class ScopedSymbolTable(val parent: ScopedSymbolTable? = null) {
    val symbols: MutableMap<String, Identifier> = hashMapOf()
}

class SymbolTable() {
    // The symbol table for the current scope (with parent pointers up to the global scope)
    private var currentTable = ScopedSymbolTable()

    // A map of identifiers to their info for all identifiers that have been seen in all scopes
    val identifiers: MutableMap<Identifier, IdentifierInfo> = hashMapOf()

    /**
     * Enter a new scope.
     */
    fun enterScope() {
        currentTable = ScopedSymbolTable(currentTable)
    }

    /**
     * Exit the current scope.
     *
     * @throws SymbolTableException if one attempts to exit the global scope
     */
    fun exitScope() {
        val parent = currentTable.parent
        if (parent == null) {
            throw SymbolTableException("Cannot exit global scope")
        }
        
        currentTable = parent
    }

    /**
     * Reset the symbol table to the global scope.
     */
    fun returnToGlobalScope() {
        var parent = currentTable.parent
        while (parent != null) {
            currentTable = parent
            parent = currentTable.parent
        }
    }

    /**
     * Lookup a string in the current scope.
     *
     * @return the identifier for the given string in the current scope, or null if no identifier
     *         with that name has been seen in the current scope
     */
    fun lookup(name: String): Identifier? {
        var table: ScopedSymbolTable? = currentTable

        // Find the lowest scope in which this name appears
        while (table != null) {
            val ident = table.symbols[name]
            if (ident != null) {
                return ident
            }

            table = table.parent
        }

        return null
    }

    /**
     * Add a new identifier to the current scope, and fill in its info.
     *
     * @param name the name of the new identifier
     * @param idClass the class of the new identifier (e.g. variable, function)
     * @param typeExpr the best known type expression for the new identifier
     * @param props an (optional) set of all properties of this identifier
     */
    fun addSymbol(
            name: String,
            idClass: IdentifierClass,
            typeExpr: TypeExpression,
            props: Set<IdentifierProperty> = hashSetOf()
    ): Identifier {
        val ident = newIdentifier(name)
        val info = IdentifierInfo(name, idClass, typeExpr, props)

        currentTable.symbols[name] = ident
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
}
