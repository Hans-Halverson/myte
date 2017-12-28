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

    override fun toString(): String {
        return "AlgebraicDataTypeSignature(name=${name}, typeParams=${typeParams}, " +
                "variants=${variants}, id=${id})"
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
 * @property name the name of the type constructor for this variant
 * @property type the (optional) type of this variant. This should only use the type variables
 *           specified as parameters in the adt signature. If null, the type constructor needs no
 *           arguments.
 * @property the unique id for this variant
 */
class AlgebraicDataTypeVariant(
    val adtSig: AlgebraicDataTypeSignature,
    val name: String,
    val typeConstructor: List<Type>,
    val id: Long = newAdtVariantId()
) {
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

    override fun hashCode(): Int = id.hashCode()

    override fun equals(other: Any?): Boolean {
        if (other !is AlgebraicDataTypeVariant) {
            return false
        }

        return id == other.id
    }
}

/**
 * Add an algebraic type signature to the global scope of the symbol table.
 */
fun addAdtSigToSymbolTable(adtSig: AlgebraicDataTypeSignature, symbolTable: SymbolTable) {
    val ident = symbolTable.addSymbolInGlobalScope(adtSig.name, IdentifierClass.ALGEBRAIC_DATA_TYPE,
            adtSig.getAdtWithParams(adtSig.typeParams))
    val info = symbolTable.getInfo(ident)!!
    info.adtSig = adtSig
}

/**
 * Add an algebraic type variant to the global scope of the symbol table.
 */
fun addAdtVariantToSymbolTable(adtVariant: AlgebraicDataTypeVariant, symbolTable: SymbolTable) {
    val ident = symbolTable.addSymbolInGlobalScope(adtVariant.name,
            IdentifierClass.ALGEBRAIC_DATA_TYPE_VARIANT, adtVariant.typeForConstructor())
    val info = symbolTable.getInfo(ident)!!
    info.adtVariant = adtVariant
}
