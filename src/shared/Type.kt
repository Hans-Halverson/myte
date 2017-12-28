package myte.shared

sealed class Type

sealed class NumberType : Type()

object UnitType : Type() {
    override fun toString(): String = "unit"
}

object BoolType : Type() {
    override fun toString(): String = "bool"
}

object StringType : Type() {
    override fun toString(): String = "string"
}

object IntType : NumberType() {
    override fun toString(): String = "int"
}

object FloatType : NumberType() {
    override fun toString(): String = "float"
}

data class VectorType(val elementType: Type) : Type() {
    override fun toString(): String = "vec<${elementType}>"
}

data class TupleType(val elementTypes: List<Type>) : Type() {
    override fun toString(): String = elementTypes.joinToString(", ", "(", ")")
}

class FunctionType(
    val argTypes: List<Type>,
    val returnType: Type
) : Type() {
    val typeParams: List<TypeParameter> = gatherTypeParameters(this)

    override fun toString(): String {
        val builder = StringBuilder()

        // Add the arg types, wrapping then in parentheses if they are function types
        if (argTypes.size > 0) {
            for (type in argTypes) {
                if (type is FunctionType) {
                    builder.append("(")
                    builder.append(type.toString())
                    builder.append(")")
                } else {
                    builder.append(type.toString())
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

    override fun equals(other: Any?): Boolean {
        if (other !is FunctionType) {
            return false
        }

        return (argTypes == other.argTypes) && (returnType == other.returnType)
    }
}

class TypeParameter(val id: Long) : Type() {
    override fun toString(): String = "${id}"

    override fun equals(other: Any?): Boolean {
        if (other !is TypeParameter) {
            return false
        }

        return (id == other.id)
    }
}

private var maxTypeParamId: Long = 0

/**
 * Return a new type parameter with the given name.
 * The type parameter id is guaranteed to be unique.
 */
fun newTypeParameter(): TypeParameter {
    if (maxTypeParamId == Long.MAX_VALUE) {
        throw Exception("Type parameter ids reached maximum value, no unique type parameters left")
    }

    return TypeParameter(maxTypeParamId++)
}

fun gatherTypeParameters(type: Type): MutableList<TypeParameter> {
    return when (type) {
        is TypeParameter -> mutableListOf(type)
        is VectorType -> gatherTypeParameters(type.elementType)
        is FunctionType -> {
            val allParams = type.argTypes.flatMap(::gatherTypeParameters).toMutableList()
                
            allParams.addAll(gatherTypeParameters(type.returnType))
            allParams.distinct().toMutableList()
        }
        else -> mutableListOf()
    }
}
