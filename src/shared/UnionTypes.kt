package myte.shared

/**
 * The signature for a union type - contains all the necessary information to construct
 * new instances of this union, and serves as the master resource for defining the union.
 *
 * @property name the name of the union
 * @property typeParams a list of type variables that correspond to the type params for this union
 * @property variants a list of all variants for this union
 */
class UnionTypeSignature(
    val name: String,
    val typeParams: List<TypeVariable>,
    val variants: MutableList<Type> = mutableListOf()
) {
    /**
     * Return a union type for this signature with fresh type parameters.
     */
    fun getFreshUnionType(): UnionType {
        val freshParams = typeParams.map { TypeVariable() }
        return getUnionTypeWithParams(freshParams)
    }

    /**
     * Return a union type for this signature with the given parameters.
     */
    fun getUnionTypeWithParams(types: List<Type>): UnionType {
        if (types.size != typeParams.size) {
            throw Exception("Type ${name} expects ${typeParams.size} type parameters, " +
                    "but received ${types.size}")
        }

        val paramsMap = typeParams.zip(types).toMap()
        val parameterizedVariants = variants.map { variant -> variant.substitute(paramsMap) }

        return UnionType(this, types, parameterizedVariants)
    }
}

/**
 * Add a union type signature to the global scope of the symbol table.
 */
fun addUnionSigToSymbolTable(unionSig: UnionTypeSignature, symbolTable: SymbolTable) {
    val ident = symbolTable.addSymbolInGlobalScope(unionSig.name, IdentifierClass.UNION_TYPE,
            unionSig.getUnionTypeWithParams(unionSig.typeParams))
    val info = symbolTable.getInfo(ident)!!
    info.unionSig = unionSig
}
