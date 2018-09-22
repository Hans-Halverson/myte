package myte.eval.values

import myte.shared.*

sealed class AlgebraicDataTypeValue(
    val adtVariant: AlgebraicDataTypeVariant,
    type: Type
) : Value(type)

class TupleVariantValue(
    adtVariant: AlgebraicDataTypeVariant,
    val fields: List<Value>,
    type: Type
) : AlgebraicDataTypeValue(adtVariant, type) {
    override fun toString(): String {
        if (fields.size > 0) {
            return "${adtVariant.name}${fields.joinToString(", ", "(", ")")}"
        } else {
            return "${adtVariant.name}"
        }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is TupleVariantValue) {
            return false
        }

        return (adtVariant.id == other.adtVariant.id && fields == other.fields)
    }

    override fun hashCode(): Int = adtVariant.id.hashCode() + fields.hashCode()
}

class RecordVariantValue(
    adtVariant: AlgebraicDataTypeVariant,
    val fields: MutableMap<String, Value>,
    type: Type
) : AlgebraicDataTypeValue(adtVariant, type) {
    override fun toString(): String {
        val fieldString = fields.map({ (fieldName, fieldValue) -> "${fieldName}: ${fieldValue}" })
                .joinToString(", ", "{ ", " }")
        return "${adtVariant.name} ${fieldString}"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is RecordVariantValue) {
            return false
        }

        return (adtVariant.id == other.adtVariant.id && fields == other.fields)
    }

    override fun hashCode(): Int = adtVariant.id.hashCode() + fields.hashCode()
}
