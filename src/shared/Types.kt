package myte.shared

/**
 * A type expression used in type inferrence.
 */
sealed class Type {
    open fun getAllVariables(): List<TypeVariable> = listOf()
}

sealed class NumberType : Type()

/**
 * A type variable in the type tree that stands in for a concrete type.
 *
 * @property id the unique id which identifies this type variable
 */
data class TypeVariable(val id: Long = newTypeVariableId()) : Type() {
    override fun getAllVariables(): List<TypeVariable> = listOf(this)

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

    override fun toString(): String = "vec<${elementType}>"
}

data class TupleType(val elementTypes: List<Type>) : Type() {
    override fun getAllVariables(): List<TypeVariable> {
        return elementTypes.map(Type::getAllVariables).flatten()
    }

    override fun toString(): String = elementTypes.joinToString(", ", "(", ")")
}

data class FunctionType(
    val argTypes: List<Type>,
    val returnType: Type
) : Type() {

    override fun getAllVariables(): List<TypeVariable> {
        val argVars = argTypes.map(Type::getAllVariables).flatten()
        val returnVars = returnType.getAllVariables()

        return listOf(argVars, returnVars).flatten()
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

//data class AlgebraicDataType(
//    val adt: AlgebraicDataType,
//    val typeParams: List<Type>
//) : Type() {
//    override fun getAllVariables(): List<TypeVariable> {
//        return typeParams.map(Type::getAllVariables).flatten()
//    }
//}
