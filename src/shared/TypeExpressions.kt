package myte.shared

sealed class TypeExpression {
	abstract fun toType(): Type?
}

sealed class NumberTypeExpression : TypeExpression()

private var maxTypeId: Long = 0

fun newTypeVariable(): TypeVariable {
	if (maxTypeId == Long.MAX_VALUE) {
		throw Exception("Type variable ids reached maximum value, no unique type variables left")
	}

	return TypeVariable(maxTypeId++)
}

data class TypeVariable(val id: Long) : TypeExpression() {
	override fun toType(): Type? = null

	override fun toString(): String = "${id}"
}

object UnitTypeExpression : TypeExpression() {
	override fun toType(): Type? = UnitType

	override fun toString(): String = "unit"
}

object BoolTypeExpression : TypeExpression() {
	override fun toType(): Type? = BoolType

	override fun toString(): String = "bool"
}

object IntTypeExpression : NumberTypeExpression() {
	override fun toType(): Type? = IntType

	override fun toString(): String = "int"
}

object FloatTypeExpression : NumberTypeExpression() {
	override fun toType(): Type? = FloatType

	override fun toString(): String = "float"
}

object StringTypeExpression : TypeExpression() {
	override fun toType(): Type? = StringType

	override fun toString(): String = "string"
}

data class ListTypeExpression(val param: TypeExpression) : TypeExpression() {
	override fun toType(): Type? {
		val paramType = param.toType()
		if (paramType == null) {
			return null
		} else {
			return ListType(paramType)
		}
	}

	override fun toString(): String = "list<${param}>"
}

data class FunctionTypeExpression(val argParams: List<TypeExpression>, val returnParam: TypeExpression) : TypeExpression() {
	override fun toType(): Type? {
		val argTypes = argParams.map { param -> param.toType() }
		val returnType = returnParam.toType()

		if (returnType == null || argTypes.any { param -> param == null} ) {
			return null
		} else {
			return FunctionType(argTypes.map { x -> x!! } , returnType)
		}
	}

	override fun toString(): String {
		val builder = StringBuilder()

		if (argParams.size > 0) {
			for (param in argParams) {
				if (param is FunctionTypeExpression) {
					builder.append("(")
					builder.append(param.toString())
					builder.append(")")
				} else {
					builder.append(param.toString())
				}

				builder.append(" -> ")
			}
		} else {
			builder.append("unit -> ")
		}

		if (returnParam is FunctionTypeExpression) {
			builder.append("(")
			builder.append(returnParam.toString())
			builder.append(")")
		} else {
			builder.append(returnParam.toString())
		}

		return builder.toString()
	}
}


fun expressionFromType(type: Type): TypeExpression {
	return when (type) {
		is UnitType -> UnitTypeExpression
		is BoolType -> BoolTypeExpression
		is StringType -> StringTypeExpression
		is IntType -> IntTypeExpression
		is FloatType -> FloatTypeExpression
		is ListType -> ListTypeExpression(expressionFromType(type.typeParam))
		is FunctionType -> FunctionTypeExpression(type.argTypes.map(::expressionFromType),
												  expressionFromType(type.returnType))
	}
}
