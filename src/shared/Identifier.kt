package myte.shared

import java.util.Random

private val identifierIdGenerator = Random()
private val identifierIds: MutableSet<Long> = mutableSetOf()

fun newIdentifier(name: String): Identifier {
	var randomId: Long = identifierIdGenerator.nextLong()

	while (identifierIds.contains(randomId)) {
		identifierIds.add(randomId)
		randomId = identifierIdGenerator.nextLong()
	}

	return Identifier(name, randomId)
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

data class IdentifierInfo(val name: String, val idClass: IdentifierClass, val type: Type, val props: Set<IdentifierProperty>)
