package myte.shared

/**
 * The signature for an algebraic data type - contains all the necessary information to construct
 * new instances of this adt, and serves as the master resource for defining the adt.
 *
 * @property name the name of the adt
 * @property typeParams a list of type variables that correspond to the type parameters for this adt
 * @property variants a list of all variants for this adt
 * @property id the unique id for this algebraic data type
 */
class AlgebraicDataTypeSignature(
    val name: String,
    val typeParams: List<TypeVariable>,
    val variants: MutableList<AlgebraicDataTypeVariant> = mutableListOf(),
    val methods: MutableMap<String, Identifier> = mutableMapOf(),
    val staticMethods: MutableMap<String, Identifier> = mutableMapOf(),
    val traits: MutableList<TraitType> = mutableListOf(),
    val id: Long = newAdtId()
) {
    /**
     * Return an algebraic data type for this signature with fresh type parameters.
     */
    fun getFreshAdt(): AlgebraicDataType {
        val freshParams = typeParams.map { TypeVariable() }
        return getAdtWithParams(freshParams)
    }

    /**
     * Return an algebraic data type for this signature with the given parameters.
     */
    fun getAdtWithParams(types: List<Type>): AlgebraicDataType {
        if (types.size != typeParams.size) {
            throw Exception("Type ${name} expects ${typeParams.size} type parameters, " +
                    "but received ${types.size}")
        }

        return AlgebraicDataType(this, types)
    }

    /**
     * Returns the set of all field and method names defined on this type, as well as the set of
     * all static method names defined on this type or its traits.
     */
    fun getAllNames(): Pair<MutableSet<String>, MutableSet<String>> {
        val methodNames: MutableSet<String> = mutableSetOf()
        val staticNames: MutableSet<String> = mutableSetOf()

        // Add fields of record if this is a simply record type
        if (variants.size == 1) {
            val firstVariant = variants[0]
            if (firstVariant is RecordVariant) {
                methodNames.addAll(firstVariant.fields.keys)
            }
        }

        // Add all method names and static method name from traits
        for (extendedTrait in traits) {
            methodNames.addAll(extendedTrait.traitSig.methodSignatures.keys)
            methodNames.addAll(extendedTrait.traitSig.concreteMethods.keys)
            staticNames.addAll(extendedTrait.traitSig.staticMethodSignatures.keys)
            staticNames.addAll(extendedTrait.traitSig.staticConcreteMethods.keys)
        }

        // Add all method names defined on this type
        methodNames.addAll(methods.keys)

        return Pair(methodNames, staticNames)
    }

    override fun hashCode(): Int = id.hashCode()

    override fun equals(other: Any?): Boolean {
        if (other !is AlgebraicDataTypeSignature) {
            return false
        }

        return id == other.id
    }
}

/**
 * A single variant of an algebraic data type - serves as the master resource for defining this
 * variant of the algebraic data type.
 *
 * @property adtSig the algebraic data type signature that this is a variant of
 * @property name the name of the type constructor for this variant
 * @property id the unique id for this variant
 */
sealed class AlgebraicDataTypeVariant(
    val adtSig: AlgebraicDataTypeSignature,
    val name: String,
    val id: Long = newAdtVariantId()
) {
    override fun hashCode(): Int = id.hashCode()

    override fun equals(other: Any?): Boolean {
        if (other !is AlgebraicDataTypeVariant) {
            return false
        }

        return id == other.id
    }
}

/**
 * A variant of an algebraic data type that is a tuple (including the variant that has no arguments)
 * 
 * @property typeConstructor a list of types representing the arguments to the type constructor
 */
class TupleVariant(
    adtSig: AlgebraicDataTypeSignature,
    name: String,
    val typeConstructor: List<Type>
) : AlgebraicDataTypeVariant(adtSig, name) {
    /**
     * Create the type corresponding to this variant's type constructor. If the constructor has
     * multiple types it is a tuple of those types, if the constructor has a single type it is
     * that type, and if the constructor has no types it is the unit type.
     */
    fun typeForConstructor(): Type {
        if (typeConstructor.size > 1) {
            return TupleType(typeConstructor)
        } else if (typeConstructor.size == 1) {
            return typeConstructor[0]
        } else {
            return UnitType
        }
    }

    /**
     * Returns a version of the type constructor with the given params substituted in for the
     * formal params in the adt signature.
     */
    fun getTypeConstructorWithParams(params: List<Type>): List<Type> {
        val paramsMap = adtSig.typeParams.zip(params).toMap()
        return typeConstructor.map { type -> type.substitute(paramsMap) }
    }

    override fun toString(): String {
        val type = typeForConstructor()
        if (type != UnitType) {
            return "${name}: ${type}"
        } else {
            return "${name}"
        }
    }
}

/**
 * A variant of an algebraic data type that is a record.
 * 
 * @property fields a map of field names to the type of that field for the record variant
 */
class RecordVariant(
    adtSig: AlgebraicDataTypeSignature,
    name: String,
    val fields: Map<String, Type>
) : AlgebraicDataTypeVariant(adtSig, name) {
    /**
     * Returns a version of the fields with the given params substituted in for the
     * formal params in the adt signature.
     */
    fun getFieldsWithParams(params: List<Type>): Map<String, Type> {
        val paramsMap = adtSig.typeParams.zip(params).toMap()
        return fields.mapValues { (_, type) -> type.substitute(paramsMap) }
    }
}
