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
    var actualArgs: List<IRNode>,
    identLocation: Location
) : IRNode(identLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        actualArgs.forEach { arg -> arg.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        actualArgs = actualArgs.map(func)
        actualArgs.forEach { arg -> arg.map(func) }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is TupleTypeConstructorNode) {
            return false
        }

        return (adtVariant == other.adtVariant && actualArgs == other.actualArgs)
    }
}
