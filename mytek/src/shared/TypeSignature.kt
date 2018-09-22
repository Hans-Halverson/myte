package myte.shared

/**
 * The signature for a type, containing all methods and traits for that type.
 *
 * @property name the name of the type
 * @property typeParams a list of type variables that correspond to the type params for this type
 * @property methods a map of all methods (keyed by their name) defined on this type
 * @property staticMethods a list of all static methods (keyed by their name) defined on this type 
 * @property traits a list of all traits that this type implements
 */
abstract class TypeSignature(
    val name: String,
    val typeParams: List<TypeParameter>,
    val methods: MutableMap<String, Identifier> = mutableMapOf(),
    val staticMethods: MutableMap<String, Identifier> = mutableMapOf(),
    val traits: MutableList<TraitType> = mutableListOf()
) {
    /**
     * Returns the set of all field and method names defined on this type, as well as the set of
     * all static method names defined on this type or its traits.
     */
    open fun getAllNames(): Pair<MutableSet<String>, MutableSet<String>> {
        val methodNames: MutableSet<String> = mutableSetOf()
        val staticNames: MutableSet<String> = mutableSetOf()

        // Add all method names and static method name from traits
        for (extendedTrait in traits) {
            methodNames.addAll(extendedTrait.traitSig.methodSignatures.keys)
            methodNames.addAll(extendedTrait.traitSig.methods.keys)
            staticNames.addAll(extendedTrait.traitSig.staticMethodSignatures.keys)
            staticNames.addAll(extendedTrait.traitSig.staticMethods.keys)
        }

        // Add all method names defined on this type
        methodNames.addAll(methods.keys)

        return Pair(methodNames, staticNames)
    }

    fun getTypeWithParams(types: List<Type>): Type {
        if (types.size != typeParams.size) {
            throw Exception("Type ${name} expects ${typeParams.size} type parameters, " +
                    "but received ${types.size}")
        }

        return createTypeWithParams(types)
    }

    abstract fun createTypeWithParams(types: List<Type>): Type
}

object TypeVariableSignature : TypeSignature("", listOf()) {
    override fun createTypeWithParams(types: List<Type>) = OpenTypeVariable()
}

object UnitTypeSignature : TypeSignature("unit", listOf()) {
    override fun createTypeWithParams(types: List<Type>) = UnitType
}

object BoolTypeSignature : TypeSignature("bool", listOf()) {
    override fun createTypeWithParams(types: List<Type>) = BoolType
}

object ByteTypeSignature : TypeSignature("byte", listOf()){
    override fun createTypeWithParams(types: List<Type>) = ByteType
}

object IntTypeSignature : TypeSignature("int", listOf()){
    override fun createTypeWithParams(types: List<Type>) = IntType
}

object FloatTypeSignature : TypeSignature("float", listOf()){
    override fun createTypeWithParams(types: List<Type>) = FloatType
}

object DoubleTypeSignature : TypeSignature("double", listOf()){
    override fun createTypeWithParams(types: List<Type>) = DoubleType
}

object StringTypeSignature : TypeSignature("string", listOf()){
    override fun createTypeWithParams(types: List<Type>) = StringType
}

object VectorTypeSignature : TypeSignature("vec", listOf(TypeParameter())){
    override fun createTypeWithParams(types: List<Type>) = VectorType(types[0])
}

object SetTypeSignature : TypeSignature("set", listOf(TypeParameter())){
    override fun createTypeWithParams(types: List<Type>) = SetType(types[0])
}

object MapTypeSignature : TypeSignature("map", listOf(TypeParameter(), TypeParameter())){
    override fun createTypeWithParams(types: List<Type>) = MapType(types[0], types[1])
}

object TupleTypeSignature : TypeSignature("function", listOf()){
    override fun createTypeWithParams(types: List<Type>) = TupleType(listOf())
}

object FunctionTypeSignature : TypeSignature("tuple", listOf()){
    override fun createTypeWithParams(types: List<Type>) = FunctionType(listOf(), UnitType)
}
