package myte.ir.nodes

import myte.shared.*

abstract class IRNode(val evalTypeExpr: TypeExpression) {
	private var typeIfInferred: Type? = null

	var type: Type
		get() = typeIfInferred!!
		set(newType: Type) {
			typeIfInferred = newType
		}

	open fun <T> map(func: (IRNode) -> T) {
		func(this)
	}
}
