package myte.shared

class TraitSignature(
    val name: String,
    val typeParams: List<TypeVariable>,
    val abstractMethods: MutableMap<String, Identifier> = mutableMapOf(),
    val concreteMethods: MutableMap<String, Identifier> = mutableMapOf(),
    val id: Long = newTraitId()
) {
    /**
     * Return a trait type for this signature with the given parameters.
     */
    fun getTraitWithParams(types: List<Type>): TraitType {
        if (types.size != typeParams.size) {
            throw Exception("Trait ${name} expects ${typeParams.size} type parameters, " +
                    "but received ${types.size}")
        }

        return TraitType(this, types)
    }

    override fun hashCode(): Int = id.hashCode()

    override fun equals(other: Any?): Boolean {
        if (other !is TraitSignature) {
            return false
        }

        return id == other.id
    }
}
