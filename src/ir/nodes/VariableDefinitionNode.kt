package myte.ir.nodes

import myte.shared.*

class VariableDefinitionNode(val ident: Identifier, val expr: IRNode) : IRNode(UnitType) {
	override fun toString(): String {
		return "VariableDefinitionNode(ident=${ident}, expr=${expr})"
	}

	override fun equals(other: Any?): Boolean {
		if (other !is VariableDefinitionNode) {
			return false
		}

		return (ident == other.ident && expr == other.expr)
	}
}

