package myte.shared

/**
 * A type expression used in type inferrence.
 */
sealed class TypeExpression {
    open fun getAllVariables(): List<TypeVariable> = listOf()
}

sealed class NumberTypeExpression : TypeExpression()

private var maxTypeId: Long = 0

/**
 * Return a new type variable with the given name. The type variable id is guaranteed to be unique.
 */
fun newTypeVariable(): TypeVariable {
    if (maxTypeId == Long.MAX_VALUE) {
        throw Exception("Type variable ids reached maximum value, no unique type variables left")
    }

    return TypeVariable(maxTypeId++)
}

/**
 * A type variable in the expression tree that stands in for a concrete type.
 *
 * @property id the unique id which identifies this type variable
 */
data class TypeVariable(val id: Long) : TypeExpression() {
    override fun getAllVariables(): List<TypeVariable> = listOf(this)

    override fun toString(): String = "${id}"
}

object UnitTypeExpression : TypeExpression() {
    override fun toString(): String = "unit"
}

object BoolTypeExpression : TypeExpression() {
    override fun toString(): String = "bool"
}

object IntTypeExpression : NumberTypeExpression() {
    override fun toString(): String = "int"
}

object FloatTypeExpression : NumberTypeExpression() {
    override fun toString(): String = "float"
}

object StringTypeExpression : TypeExpression() {
    override fun toString(): String = "string"
}

data class ListTypeExpression(val elementType: TypeExpression) : TypeExpression() {
    override fun getAllVariables(): List<TypeVariable> = elementType.getAllVariables()

    override fun toString(): String = "list<${elementType}>"
}

data class TupleTypeExpression(val elementTypes: List<TypeExpression>) : TypeExpression() {
    override fun getAllVariables(): List<TypeVariable> {
        return elementTypes.map(TypeExpression::getAllVariables).flatten()
    }

    override fun toString(): String = elementTypes.joinToString(", ", "(", ")")
}

data class FunctionTypeExpression(
    val argTypes: List<TypeExpression>,
    val returnType: TypeExpression
) : TypeExpression() {

    override fun getAllVariables(): List<TypeVariable> {
        val argVars = argTypes.map(TypeExpression::getAllVariables).flatten()
        val returnVars = returnType.getAllVariables()

        return listOf(argVars, returnVars).flatten()
    }

    override fun toString(): String {
        val builder = StringBuilder()

        // Add the arg types, wrapping then in parentheses if they are function types
        if (argTypes.size > 0) {
            for (argType in argTypes) {
                if (argType is FunctionTypeExpression) {
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
        if (returnType is FunctionTypeExpression) {
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
 * Convert a type to the equivalent type expression.
 */
fun expressionFromType(type: Type): TypeExpression {
    return when (type) {
        is UnitType -> UnitTypeExpression
        is BoolType -> BoolTypeExpression
        is StringType -> StringTypeExpression
        is IntType -> IntTypeExpression
        is FloatType -> FloatTypeExpression
        is ListType -> ListTypeExpression(expressionFromType(type.elementType))
        is TupleType -> TupleTypeExpression(type.elementTypes.map(::expressionFromType))
        is FunctionType -> FunctionTypeExpression(type.argTypes.map(::expressionFromType),
                                                  expressionFromType(type.returnType))
        is TypeParameter -> newTypeVariable()
    }
}
