package myte.shared

/**
 * A type expression used in type inferrence.
 */
sealed class Type {
    /**
     * Return a list of all type variables contained in this type and its child types.
     */
    open fun getAllVariables(): List<TypeVariable> = listOf()

    /**
     * Return a new type that is identical to this type, with every type variable in the supplied
     * map mapped to the corresponding type.
     */
    open fun substitute(typeMap: Map<TypeVariable, Type>): Type = this
}

sealed class NumberType : Type()

/**
 * A type variable in the type tree that stands in for a concrete type.
 *
 * @property id the unique id which identifies this type variable
 */
data class TypeVariable(val id: Long = newTypeVariableId()) : Type() {
    override fun getAllVariables(): List<TypeVariable> = listOf(this)

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        val mappedType = typeMap[this]
        return mappedType ?: this
    }

    override fun toString(): String = "${id}"
}

object UnitType : Type() {
    override fun toString(): String = "unit"
}

object BoolType : Type() {
    override fun toString(): String = "bool"
}

object IntType : NumberType() {
    override fun toString(): String = "int"
}

object FloatType : NumberType() {
    override fun toString(): String = "float"
}

object StringType : Type() {
    override fun toString(): String = "string"
}

data class VectorType(val elementType: Type) : Type() {
    override fun getAllVariables(): List<TypeVariable> = elementType.getAllVariables()

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        return VectorType(elementType.substitute(typeMap))
    }

    override fun toString(): String = "vec<${elementType}>"
}

data class TupleType(val elementTypes: List<Type>) : Type() {
    override fun getAllVariables(): List<TypeVariable> {
        return elementTypes.map(Type::getAllVariables).flatten()
    }

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        return TupleType(elementTypes.map { elementType -> elementType.substitute(typeMap) })
    }

    override fun toString(): String = elementTypes.joinToString(", ", "(", ")")
}

data class FunctionType(
    val argTypes: List<Type>,
    val returnType: Type
) : Type() {

    override fun getAllVariables(): List<TypeVariable> {
        val argVals = argTypes.map(Type::getAllVariables).flatten()
        val returnVal = returnType.getAllVariables()

        return listOf(argVals, returnVal).flatten()
    }

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        val argVals = argTypes.map({ argType -> argType.substitute(typeMap) })
        val returnVal = returnType.substitute(typeMap)

        return FunctionType(argVals, returnVal)
    }

    override fun toString(): String {
        val builder = StringBuilder()

        // Add the arg types, wrapping then in parentheses if they are function types
        if (argTypes.size > 0) {
            for (argType in argTypes) {
                if (argType is FunctionType) {
                    builder.append("(")
                    builder.append(argType.toString())
                    builder.append(")")
                } else {
                    builder.append(argType.toString())
                }

                builder.append(" -> ")
            }
        } else {
            builder.append("unit -> ")
        }

        // Add the return type, wrapping it in parentheses if it is a function type
        if (returnType is FunctionType) {
            builder.append("(")
            builder.append(returnType.toString())
            builder.append(")")
        } else {
            builder.append(returnType.toString())
        }

        return builder.toString()
    }
}

/**
 * An instance of an algebraic data type that has been parameterized by actual types.
 *
 * @property adtSig the algebraic data type signature for this adt
 * @property typeParams a list of the actual type parameters supplied to this instance of the adt
 */
data class AlgebraicDataType(
    val adtSig: AlgebraicDataTypeSignature,
    val typeParams: List<Type>
) : Type() {
    override fun getAllVariables(): List<TypeVariable> {
        return typeParams.map(Type::getAllVariables).flatten()
    }

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        val substTypes = typeParams.map { typeParam -> typeParam.substitute(typeMap) }
        return AlgebraicDataType(adtSig, substTypes)
    }

    override fun toString(): String {
        val builder = StringBuilder()

        builder.append(adtSig.name)

        if (typeParams.size > 0) {
            builder.append(typeParams.joinToString(", ", "<", ">"))
        }

        return builder.toString()
    }
}
