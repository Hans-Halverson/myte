package myte.eval.values

import myte.shared.*

class AlgebraicDataTypeValue(
    val adtVariant: AlgebraicDataTypeVariant,
    val fields: List<Value>,
    type: Type
) : Value(type) {
    override fun toString(): String {
        if (fields.size > 0) {
            return "${adtVariant.name}${fields.joinToString(", ", "(", ")")}"
        } else {
            return "${adtVariant.name}"
        }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is AlgebraicDataTypeValue) {
            return false
        }

        return (adtVariant.id == other.adtVariant.id &&
                adtVariant.typeConstructor == other.adtVariant.typeConstructor)
    }
}
