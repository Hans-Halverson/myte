package myte.ir.nodes

import myte.shared.*

class AssignmentNode(val ident: Identifier, val expr: IRNode, type: Type) : IRNode(type) {
	override fun toString(): String {
		return "AssignmentNode(ident=${ident}, expr=${expr}, type=${type})"
	}

	override fun equals(other: Any?): Boolean {
		if (other !is AssignmentNode) {
			return false
		}

		return (ident == other.ident && expr == other.expr)
	}
}
