package myte.ir.nodes

import myte.shared.*

data class ReturnNode(val expr: IRNode?) : IRNode(UnitTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		expr?.map(func)
	}
}
