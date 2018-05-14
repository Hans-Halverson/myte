package myte.eval.builtins

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
    override fun eval(args: List<Value>, recv: Value): Value {
        if (recv is TupleVariantValue) {
            return StringValue(recv.toString())
        } else  {
            return StringValue((recv as RecordVariantValue).toString())
        }
    }
}
