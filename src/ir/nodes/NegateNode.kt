package myte.ir.nodes

import myte.shared.*

class NegateNode(val expr: IRNode) : IRNode(FloatTypeExpression) {
	override fun <T> map(func: (IRNode) -> T) {
		func(this)
		expr.map(func)
	}
}
