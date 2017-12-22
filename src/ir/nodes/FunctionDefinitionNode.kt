package myte.ir.nodes

import myte.shared.*

data class FunctionDefinitionNode(val ident: Identifier, val formalArgs: List<Identifier>, val body: IRNode) : IRNode(UnitTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		body.map(func)
	}
}
