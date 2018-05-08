package myte.shared

/**
 * A symbol in the program that has a specific meaning (variable, function, etc).
 *
 * @property name the name of this identifier in the program
 * @property id the unique id which identifies this identifier
 */
data class Identifier(val name: String, val id: Long = newIdentifierId())

// An IdentifierClass is the unique category of object that an identifier represents.
enum class IdentifierClass {
    VARIABLE,
    FUNCTION,
    TYPE_PARAMETER,
    ALGEBRAIC_DATA_TYPE,
    ALGEBRAIC_DATA_TYPE_VARIANT,
    TRAIT
}

// An IdentifierProperty is a possible property of an identifier.
// A single identifier can have many IdentifierProperties.
enum class IdentifierProperty {
    IMMUTABLE,
}

/**
 * A structure that captures all information about a particular identifier.
 *
 * @property name the name of the identifier in the program
 * @property idClass the class of the identifier (e.g. variable, function)
 * @property location the location of the identifier in the source code
 * @property props a set of all properties of this identifier
 */
data class IdentifierInfo(
    val name: String,
    val idClass: IdentifierClass,
    val location: Location,
    val props: Set<IdentifierProperty>
) {
    lateinit var type: Type
    lateinit var adtSig: AlgebraicDataTypeSignature
    lateinit var adtVariant: AlgebraicDataTypeVariant
    lateinit var traitSig: TraitSignature

    var typeShouldBeInferred = false
    var typeIsInferred = false
}
