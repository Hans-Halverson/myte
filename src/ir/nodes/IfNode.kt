package myte.ir.nodes

import myte.shared.*

data class IfNode(val cond: IRNode, val conseq: IRNode, val altern: IRNode?) : IRNode(UnitTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		cond.map(func)
		conseq.map(func)
		altern?.map(func)
	}
}
