package myte.shared

private var maxIdentId: Long = 0

fun newIdentifier(name: String): Identifier {
	if (maxIdentId == Long.MAX_VALUE) {
		throw Exception("Identifier reached maximum value, no unique identifiers left")
	}

	return Identifier(name, maxIdentId++)
}

data class Identifier(val name: String, val id: Long)

enum class IdentifierClass {
	VARIABLE,
	FUNCTION,
}

enum class IdentifierProperty {
	NUMERIC,
	IMMUTABLE,
}

data class IdentifierInfo(val name: String, val idClass: IdentifierClass, val typeExpr: TypeExpression, val props: Set<IdentifierProperty>) {
	private var typeIfInferred: Type? = null

	var type: Type
		get() = typeIfInferred!!
		set(newType: Type) {
			typeIfInferred = newType
		}
}
