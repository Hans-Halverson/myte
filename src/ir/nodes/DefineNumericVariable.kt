package myte.ir.nodes

import myte.shared.*

data class DefineNumericVariable(val ident: Identifier, val expr: IRNumericNode) : IRNode()
