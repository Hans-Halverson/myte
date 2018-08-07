package myte.ir.nodes

import myte.shared.*

/**
 * A node that represents a trait definition.
 *
 * @property traitSig the trait signature for the trait created in this definition
 * @property methods all methods defined in this trait definition
 */
class TraitDefinitionNode(
    var traitSig: TraitSignature,
    var methods: List<IRNode>,
    startLocation: Location
) : IRNode(startLocation) {
    override fun <T> forEach(func: (IRNode) -> T) {
        func(this)
        methods.forEach { method -> method.forEach(func) }
    }

    override fun map(func: (IRNode) -> IRNode) {
        methods = methods.map(func)
        methods.forEach { method -> method.map(func) }
    }
}
