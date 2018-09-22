package myte.shared

private enum class IdType {
    ALGEBRAIC_DATA_TYPE,
    ALGEBRAIC_DATA_TYPE_VARIANT,
    IDENTIFIER,
    TRAIT,
    TYPE_VARIABLE
}

private val maxTypeIds: MutableMap<IdType, Long> = mutableMapOf(
    IdType.ALGEBRAIC_DATA_TYPE to 0L,
    IdType.ALGEBRAIC_DATA_TYPE_VARIANT to 0L,
    IdType.IDENTIFIER to 0L,
    IdType.TRAIT to 0L,
    IdType.TYPE_VARIABLE to 0L
)

/**
 * Return a new, unique id for the give id type.
 */
private fun newId(idType: IdType): Long {
    val nextId = maxTypeIds[idType]
    if (nextId == Long.MAX_VALUE) {
        throw Exception("${idType} ids reached maximum value, no unique ids left")
    }

    maxTypeIds[idType] = nextId!! + 1

    return nextId
}

fun newIdentifierId(): Long = newId(IdType.IDENTIFIER)

fun newAdtId(): Long = newId(IdType.ALGEBRAIC_DATA_TYPE)

fun newAdtVariantId(): Long = newId(IdType.ALGEBRAIC_DATA_TYPE_VARIANT)

fun newTraitId(): Long = newId(IdType.TRAIT)

fun newTypeVariableId(): Long = newId(IdType.TYPE_VARIABLE)
