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

    /**
     * Convert this type into its canonical form, removing all GroupTypes.
     */
    open fun canonicalize(): Type = this

    /**
     * Convert this type into its string representation, where the string representation for each
     * type variable comes from the included map.
     */
    abstract fun formatToString(typeVars: Map<TypeVariable, String> = mapOf()): String
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

    override fun formatToString(typeVars: Map<TypeVariable, String>): String {
        return typeVars[this] ?: "${id}"
    }

    override fun toString(): String = formatToString()
}

object UnitType : Type() {
    override fun formatToString(typeVars: Map<TypeVariable, String>): String = "unit"

    override fun toString(): String = formatToString()
}

object BoolType : Type() {
    override fun formatToString(typeVars: Map<TypeVariable, String>): String = "bool"

    override fun toString(): String = formatToString()
}

object IntType : NumberType() {
    override fun formatToString(typeVars: Map<TypeVariable, String>): String = "int"

    override fun toString(): String = formatToString()
}

object FloatType : NumberType() {
    override fun formatToString(typeVars: Map<TypeVariable, String>): String = "float"

    override fun toString(): String = formatToString()
}

object StringType : Type() {
    override fun formatToString(typeVars: Map<TypeVariable, String>): String = "string"

    override fun toString(): String = formatToString()
}

data class VectorType(val elementType: Type) : Type() {
    override fun getAllVariables(): List<TypeVariable> = elementType.getAllVariables()

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        return VectorType(elementType.substitute(typeMap))
    }

    override fun canonicalize(): Type = VectorType(elementType.canonicalize())

    override fun formatToString(typeVars: Map<TypeVariable, String>): String {
        return "vec<${elementType.formatToString(typeVars)}>"
    }

    override fun toString(): String = formatToString()
}

data class TupleType(val elementTypes: List<Type>) : Type() {
    override fun getAllVariables(): List<TypeVariable> {
        return elementTypes.map(Type::getAllVariables).flatten()
    }

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        return TupleType(elementTypes.map { elementType -> elementType.substitute(typeMap) })
    }

    override fun canonicalize(): Type = TupleType(elementTypes.map(Type::canonicalize))

    override fun formatToString(typeVars: Map<TypeVariable, String>): String {
        return elementTypes.map { elementType -> elementType.formatToString(typeVars) }
                .joinToString(", ", "(", ")")
    }

    override fun toString(): String = formatToString()
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

    override fun canonicalize(): Type =
            FunctionType(argTypes.map(Type::canonicalize), returnType.canonicalize())

    override fun formatToString(typeVars: Map<TypeVariable, String>): String {
        val builder = StringBuilder()

        // Add the arg types, wrapping then in parentheses if they are function types
        if (argTypes.size > 0) {
            for (argType in argTypes) {
                if (argType is FunctionType) {
                    builder.append("(")
                    builder.append(argType.formatToString(typeVars))
                    builder.append(")")
                } else {
                    builder.append(argType.formatToString(typeVars))
                }

                builder.append(" -> ")
            }
        } else {
            builder.append("unit -> ")
        }

        // Add the return type, wrapping it in parentheses if it is a function type
        if (returnType is FunctionType) {
            builder.append("(")
            builder.append(returnType.formatToString(typeVars))
            builder.append(")")
        } else {
            builder.append(returnType.formatToString(typeVars))
        }

        return builder.toString()
    }

    override fun toString(): String = formatToString()
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

    override fun canonicalize(): Type =
            AlgebraicDataType(adtSig, typeParams.map(Type::canonicalize))

    override fun formatToString(typeVars: Map<TypeVariable, String>): String {
        val builder = StringBuilder()

        builder.append(adtSig.name)

        if (typeParams.size > 0) {
            builder.append(typeParams.map { typeParam -> typeParam.formatToString(typeVars) }
                    .joinToString(", ", "<", ">"))
        }

        return builder.toString()
    }

    override fun toString(): String = formatToString()
}

/**
 * An instance of a union type that has been parameterized by actual types.
 *
 * @property variants a list of all variants for this union
 * @property typeParams a list of the actual type parameters that this union is parameterized by.
 *           this should be nonempty only if a this is a defined union type with a unionSig
 * @property unionSig the union type signature for this union, if it has been defined
 */
data class UnionType(
    val variants: List<Type>,
    val typeParams: List<Type> = listOf(),
    val unionSig: UnionTypeSignature? = null
) : Type() {
    override fun getAllVariables(): List<TypeVariable> {
        return typeParams.map(Type::getAllVariables).flatten() +
                variants.map(Type::getAllVariables).flatten()
    }

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type {
        val newTypeParams = typeParams.map { typeParam -> typeParam.substitute(typeMap) }
        val newVariants = variants.map { variant -> variant.substitute(typeMap) }

        return UnionType(newTypeParams, newVariants, unionSig)
    }

    override fun canonicalize(): Type {
        val canonicalVariants = variants.map(Type::canonicalize)
        val canonicalParams = typeParams.map(Type::canonicalize)
        
        return UnionType(canonicalVariants, canonicalParams, unionSig)
    }

    override fun formatToString(typeVars: Map<TypeVariable, String>): String {
        val builder = StringBuilder()

        if (unionSig != null) {
            builder.append(unionSig.name)

            if (typeParams.size > 0) {
                builder.append(typeParams.map { typeParam -> typeParam.formatToString(typeVars) }
                        .joinToString(", ", "<", ">"))
            }
        } else {
                builder.append(variants.map { variant -> variant.formatToString(typeVars) }
                        .joinToString(" | "))
        }

        return builder.toString()
    }

    override fun toString(): String = formatToString()
}

/**
 * A type that wraps another type while parsing. This is a dummy type, and should not exist in any
 * type after parsing, as it is removed by canonicalization.
 */
class GroupType(val type: Type) : Type() {
    override fun getAllVariables(): List<TypeVariable> =
            throw Exception("Group type is unimplemented")

    override fun substitute(typeMap: Map<TypeVariable, Type>): Type =
            throw Exception("Group type is unimplemented")

    override fun canonicalize() = type

    override fun formatToString(typeVars: Map<TypeVariable, String>): String =
            throw Exception("Group type is unimplemented")
}

/**
 * Format a list of types into strings, where type variables of the same type are replaced by
 * the same representations. The representation of these type variables will be:
 * a, b, c, ..., a2, b2, c2, ... a3, b3, c3, ... etc.
 */
fun formatTypes(types: List<Type>): List<String> {
    val allVariables = types.flatMap(Type::getAllVariables).distinct()
    val varToStr: MutableMap<TypeVariable, String> = mutableMapOf()

    allVariables.forEachIndexed { index, typeVar ->
        val quot = index / 26
        val rem = index % 26
        // If on the first 26 type variables, just use 'a' ... 'z'
        val stringRep = if (quot == 0) {
            'a'.plus(rem).toString()
        // All later type variables have a number appended (e.g. a2)
        } else {
            "${'a'.plus(rem)}${quot + 1}"
        }

        varToStr[typeVar] = stringRep
    }

    return types.map { type -> type.formatToString(varToStr)}
}

/**
 * Format a single type into a string.
 */
fun formatType(type: Type): String = formatTypes(listOf(type))[0]
