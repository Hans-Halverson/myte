package myte.ir.nodes

import myte.shared.*

data class DoWhileNode(val cond: IRNode, val body: IRNode) : IRNode(UnitTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		cond.map(func)
		body.map(func)
	}
}
