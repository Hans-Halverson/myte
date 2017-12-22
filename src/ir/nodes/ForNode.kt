package myte.ir.nodes

import myte.shared.*

data class ForNode(val init: IRNode?, val cond: IRNode?, val update: IRNode?, val body: IRNode) : IRNode(UnitTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		init?.map(func)
		cond?.map(func)
		update?.map(func)
		body.map(func)
	}
}
