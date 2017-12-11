package myte.ir.nodes

import myte.shared.*

data class DefineNumericFunction(val ident: Identifier, val formalArgs: List<Identifier>, val expr: IRNumericNode) : IRNode()
