package myte.shared

class TraitSignature(
    name: String,
    typeParams: List<TypeParameter>,
    val methodSignatures: MutableMap<String, Identifier> = mutableMapOf(),
    methods: MutableMap<String, Identifier> = mutableMapOf(),
    val staticMethodSignatures: MutableMap<String, Identifier> = mutableMapOf(),
    staticMethods: MutableMap<String, Identifier> = mutableMapOf(),
    val id: Long = newTraitId()
) : TypeSignature(name, typeParams, methods, staticMethods, mutableListOf()) {
    
    override fun createTypeWithParams(types: List<Type>): TraitType = TraitType(this, types)

    override fun hashCode(): Int = id.hashCode()

    override fun equals(other: Any?): Boolean {
        if (other !is TraitSignature) {
            return false
        }

        return id == other.id
    }
}
