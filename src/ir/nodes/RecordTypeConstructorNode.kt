package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a record type constructor called on a map of fields.
 *
 * @property adtVariant the algebraic data type variant that this is a constructor for
 * @property func the algebraic data type variant that this is a type constructor for
 * @property fields a map of field names to the value they should be set to for this record
 */
class RecordTypeConstructorNode(
    val adtVariant: RecordVariant,
    val fields: Map<String, IRNode>,
    identLocation: Location
) : IRNode(identLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        fields.values.map { arg -> arg.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is RecordTypeConstructorNode) {
            return false
        }

        return (adtVariant == other.adtVariant && fields == other.fields)
    }
}
