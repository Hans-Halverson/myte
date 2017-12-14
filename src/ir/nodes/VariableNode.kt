package myte.ir.nodes

import myte.shared.*

class VariableNode(val ident: Identifier, type: Type): IRNode(type) {
	override fun toString(): String {
		return "VariableNode(ident=${ident}, type=${type})"
	}

	override fun equals(other: Any?): Boolean {
		if (other !is VariableNode) {
			return false
		}

		return ident == other.ident
	}
}

