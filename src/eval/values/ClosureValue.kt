package myte.eval.values

import myte.ir.nodes.*
import myte.shared.*

class ClosureValue(
    val ident: Identifier,
    val formalArgs: List<Identifier>,
    val body: IRNode,
    val environment: Environment,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "${ident.name}: ${type}"
}
