package myte.ir.nodes

import myte.shared.*

class ListLiteralNode(val elements: List<IRNode>) : IRNode(ListTypeExpression(newTypeVariable())) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		elements.map { element -> element.map(func) }
	}
}
