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
    var fields: Map<String, IRNode>,
    identLocation: Location
) : IRNode(identLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        fields.values.forEach { arg -> arg.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        fields = fields.mapValues { (_, value) -> func(value) }
        fields.forEach { (_, value) -> value.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is RecordTypeConstructorNode) {
            return false
        }

        return (adtVariant == other.adtVariant && fields == other.fields)
    }
}
