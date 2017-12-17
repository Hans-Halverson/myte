package myte.shared

sealed class Type

sealed class NumberType : Type()

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

class FunctionType(val argTypes: List<Type>, val returnType: Type) : Type() {
	override fun toString(): String {
		val builder = StringBuilder()

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
