package myte.ir.nodes

import myte.shared.*

class FunctionCallNode(val func: Identifier, val actualArgs: List<IRNode>, evalTypeExpr: TypeExpression) : IRNode(evalTypeExpr) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		actualArgs.map { arg -> arg.map(func) }
	}

	override fun toString(): String {
		return "FunctionCallNode(func=${func}, actualArgs=${actualArgs})"
	}

	override fun equals(other: Any?): Boolean {
		if (other !is FunctionCallNode) {
			return false
		}

		return (func == other.func && actualArgs == other.actualArgs)
	}
}
