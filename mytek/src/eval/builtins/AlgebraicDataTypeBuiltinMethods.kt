package myte.eval.builtins

import myte.eval.*
import myte.eval.values.*
import myte.shared.*

/**
 * A builtin which converts an ADT to a string.
 */
object AlgebraicDataTypeToStringBuiltinMethod {
    /**
    * Converts an ADT to a string.
    */
    fun eval(
        @Suppress("UNUSED_PARAMETER") args: List<Value>,
        recv: Value,
        env: Environment,
        eval: Evaluator
    ): Value {
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

                return StringValue("${receiver.adtVariant.name} ${fieldsString}")
            }
        }
    }
}
