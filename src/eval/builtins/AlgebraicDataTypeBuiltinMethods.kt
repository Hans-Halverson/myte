package myte.eval.builtins

import myte.eval.*
import myte.eval.values.*
import myte.shared.*

const val ALGEBRAIC_DATA_TYPE_TO_STRING_METHOD = "toString"

val ALGEBRAIC_DATA_TYPE_BUILTIN_METHODS: Map<String, BuiltinMethod> = hashMapOf(
    ALGEBRAIC_DATA_TYPE_TO_STRING_METHOD to AlgebraicDataTypeToStringBuiltinMethod()
)

/**
 * A builtin which converts an ADT to a string.
 */
class AlgebraicDataTypeToStringBuiltinMethod(
) : BuiltinMethod(
    ALGEBRAIC_DATA_TYPE_TO_STRING_METHOD,
    FunctionType(listOf(), StringType),
    UnitType
) {
    /**
    * Converts an ADT to a string.
    */
    override fun eval(args: List<Value>, recv: Value, env: Environment, eval: Evaluator): Value {
        val receiver = recv as AlgebraicDataTypeValue
        when (receiver) {
            is TupleVariantValue -> {
                if (receiver.fields.isEmpty()) {
                    return StringValue(receiver.adtVariant.name)
                } else {
                    val fieldsString = receiver.fields.map({ field ->
                        callToString(field, env, eval).str
                    }).joinToString(", ", "(", ")")

                    return StringValue("${receiver.adtVariant.name}${fieldsString}")
                }
            }
            is RecordVariantValue -> {
                val fieldsString = receiver.fields.map({ (fieldName, fieldValue) ->
                    "${fieldName}: ${callToString(fieldValue, env, eval).str}"
                }).joinToString(", ", "{ ", " }")

                return StringValue("${receiver.adtVariant.name}${fieldsString}")
            }
        }
    }
}
