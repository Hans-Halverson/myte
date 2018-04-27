package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a tuple type constructor called on an optional list of arguments.
 *
 * @property adtVariant the algebraic data type variant that this is a constructor for
 * @property func the algebraic data type variant that this is a type constructor for
 * @property actualArgs the expressions that evaluate to the arguments of the
 *           type constructor
 */
class TupleTypeConstructorNode(
    val adtVariant: TupleVariant,
    val actualArgs: List<IRNode>,
    identLocation: Location
) : IRNode(identLocation) {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        actualArgs.map { arg -> arg.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is TupleTypeConstructorNode) {
            return false
        }

        return (adtVariant == other.adtVariant && actualArgs == other.actualArgs)
    }
}
