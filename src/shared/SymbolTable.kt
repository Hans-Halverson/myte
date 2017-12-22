package myte.shared

class SymbolTableException(message: String) : Exception(message)

private class ScopedSymbolTable(val parent: ScopedSymbolTable? = null) {
	val symbols: MutableMap<String, Identifier> = hashMapOf()
}

class SymbolTable() {
	private var currentTable = ScopedSymbolTable()
	val identifiers: MutableMap<Identifier, IdentifierInfo> = hashMapOf()

	fun enterScope() {
		currentTable = ScopedSymbolTable(currentTable)
	}

	fun exitScope() {
		val parent = currentTable.parent
		if (parent == null) {
			throw SymbolTableException("Cannot exit global scope")
		}
		
		currentTable = parent
	}

	fun returnToGlobalScope() {
		var parent = currentTable.parent
		while (parent != null) {
			currentTable = parent
			parent = currentTable.parent
		}
	}

	fun lookup(name: String): Identifier? {
		var table: ScopedSymbolTable? = currentTable

		while (table != null) {
			val ident = table.symbols[name]
			if (ident != null) {
				return ident
			}

			table = table.parent
		}

		return null
	}

	fun addSymbol(name: String, idClass: IdentifierClass, typeExpr: TypeExpression, props: Set<IdentifierProperty> = hashSetOf()): Identifier {
		val ident = newIdentifier(name)
		val info = IdentifierInfo(name, idClass, typeExpr, props)

		currentTable.symbols[name] = ident
		identifiers[ident] = info

		return ident
	}

	fun getInfo(ident: Identifier): IdentifierInfo? {
		return identifiers[ident]
	}
}
