package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a type constructor called on an optional list of arguments.
 *
 * @property func the algebraic data type variant that this is a type constructor for
 * @property actualArgs the expressions that evaluate to the arguments of the
 *           type constructor
 */
class TypeConstructorNode(
    val adtVariant: AlgebraicDataTypeVariant,
    val actualArgs: List<IRNode>
) : IRNode() {
    override fun <T> map(func: (IRNode) -> T) {
        func(this)
        actualArgs.map { arg -> arg.map(func) }
    }

    override fun toString(): String {
        return "TypeConstructorNode(adtVariant=${adtVariant}, actualArgs=${actualArgs})"
    }

    override fun equals(other: Any?): Boolean {
        if (other !is TypeConstructorNode) {
            return false
        }

        return (adtVariant == other.adtVariant && actualArgs == other.actualArgs)
    }
}
