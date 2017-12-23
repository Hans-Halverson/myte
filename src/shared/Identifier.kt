package myte.shared

private var maxIdentId: Long = 0

/**
 * Return a new identifier with the given name. The identifier id is guaranteed to be unique.
 */
fun newIdentifier(name: String): Identifier {
    if (maxIdentId == Long.MAX_VALUE) {
        throw Exception("Identifier reached maximum value, no unique identifiers left")
    }

    return Identifier(name, maxIdentId++)
}

/**
 * A symbol in the program that has a specific meaning (variable, function, etc).
 *
 * @property name the name of this identifier in the program
 * @property id the unique id which identifies this identifier
 */
data class Identifier(val name: String, val id: Long)

// An IdentifierClass is the unique category of object that an identifier represents.
enum class IdentifierClass {
    VARIABLE,
    FUNCTION,
}

// An IdentifierProperty is a possible property of an identifier.
// A single identifier can have many IdentifierProperties.
enum class IdentifierProperty {
    NUMERIC,
    IMMUTABLE,
}

/**
 * A structure that captures all information about a particular identifier.
 *
 * @property name the name of the identifier in the program
 * @property idClass the class of the identifier (e.g. variable, function)
 * @property typeExpr the best known type expression for this identifier when it is initially
 *           parsed
 * @property props a set of all properties of this identifier
 */
data class IdentifierInfo(
    val name: String,
    val idClass: IdentifierClass,
    val typeExpr: TypeExpression,
    val props: Set<IdentifierProperty>
) {
    private var typeIfInferred: Type? = null

    /**
     * @property type the inferred type of this identifier. Only acces this property once type
     *           inference has been completed.
     */
    var type: Type
        get() = typeIfInferred!!
        set(newType: Type) {
            typeIfInferred = newType
        }
}
